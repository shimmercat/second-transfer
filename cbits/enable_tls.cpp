
#ifdef INCLUDE_BOTAN_ALL_H
#include "botan_all.h"
#else

#include <string>
#include <iostream>
#include <sstream>

#include <botan/botan.h>
#include <botan/tls_session.h>
#include <botan/tls_alert.h>
#include <botan/tls_policy.h>
#include <botan/tls_exceptn.h>
#include <botan/exceptn.h>
#include <botan/credentials_manager.h>
#include <botan/tls_channel.h>
#include <botan/pkcs8.h>
#include <botan/tls_session_manager.h>
#include <botan/tls_server.h>
#include <botan/data_src.h>
#endif

 #include "HsFFI.h"

#include <functional>

#include <cstdlib>

#if defined _WIN32 || defined __CYGWIN__
   #ifdef BUILDING_DLL
       #ifdef __GNUC__
          #define DLL_PUBLIC __attribute__ ((dllexport))
       #else
          #define DLL_PUBLIC __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
       #endif
   #else
       #ifdef __GNUC__
          #define DLL_PUBLIC __attribute__ ((dllimport))
       #else
          #define DLL_PUBLIC __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
       #endif
   #endif
   #define DLL_LOCAL
#else
   #if __GNUC__ >= 4
      #define DLL_PUBLIC __attribute__ ((visibility ("default")))
      #define DLL_LOCAL  __attribute__ ((visibility ("hidden")))
   #else
      #define DLL_PUBLIC
      #define DLL_LOCAL
   #endif
#endif

namespace second_transfer {


enum protocol_strategy_enum_t {
    PREFER_HTTP11 = 1,
    PREFER_HTTP2 = 2
};

enum chosen_protocol_t{
    NOCHOSENYET_CHP = 0,
    HTTP11_CHP = 1,
    HTTP2_CHP = 2
};


struct buffers_t{
    char* enc_cursor;
    char* enc_end;

    char* clr_cursor;
    char* clr_end;

    // Let's have some space for the protocols
    protocol_strategy_enum_t strategy;
    chosen_protocol_t chosen_protocol;

    // And a pointer to the engine.
    Botan::TLS::Channel* channel;
    // Has the handshake completed?
    bool handshake_completed;
    // Has an alert been produced?
    bool alert_produced;
    // Did the peer closed the transport?
    bool peer_closed_transport;

    buffers_t(
        protocol_strategy_enum_t strategy
    ):
        enc_cursor(0),
        enc_end(0),
        clr_cursor(0),
        clr_end(0),
        alert_produced(false),
        handshake_completed(false),
        chosen_protocol(NOCHOSENYET_CHP),
        channel(0),
        strategy(strategy),
        peer_closed_transport(false)
    {
    }

    void clear_cursors()
    {
        enc_cursor = 0;
        enc_end = 0;

        clr_cursor = 0;
        clr_end = 0;
    }

    ~buffers_t(){
        delete channel;
    }
};


class buffer_override_exception_t: public std::exception {
public:
    enum SituationEnum {
        GEN_CLEARTEXT=0,
        GEN_CIPHER = 1
    };
    SituationEnum situation;
    buffer_override_exception_t( SituationEnum i ): situation(i) {}
    virtual const char* what() const noexcept (true)
    {
        if ( situation == 0 )
        {
            return "Buffer override generating cleartext";
        }
        else {
            return "Buffer override generating ciphertext";
        }
    }
};


// Invoked by botan to deposit encrypted data to send to the counter-party
void output_dn_cb (buffers_t* buffers, const unsigned char a[], size_t sz)
{
    //iocba_push(botan_pad_ref, (char*)a, sz);
    if ( ( buffers->enc_cursor + sz) > buffers -> enc_end )
    {
        printf("output_dn_cb enc_cursor %p enc_end %p \n", 
               buffers->enc_cursor, 
               buffers->enc_end
               );
        throw buffer_override_exception_t(
               buffer_override_exception_t::GEN_CIPHER
               );
    }

    // DEBUG
    //for(int j = 0; j < sz; j++)
        //printf("%02X ", a[j]);
    //printf("\n");
    // END DEBUG

    memcpy(buffers->enc_cursor, (const char*) a, sz);
    buffers->enc_cursor += sz;
}

// Invoked by botan to provide clear text
void data_cb (buffers_t* buffers, const unsigned char a[], size_t sz)
{
    //iocba_data_cb(botan_pad_ref, (char*)a, sz);
    if ( buffers->clr_cursor + sz > buffers -> clr_end )
    {
        throw buffer_override_exception_t(buffer_override_exception_t::GEN_CIPHER);
    }
    memcpy(buffers->clr_cursor, (const char*) a, sz);
    buffers->clr_cursor += sz;
}

void alert_cb (buffers_t* buffers, Botan::TLS::Alert const& alert, const unsigned char a[], size_t sz) 
{
    // printf("BOTAN WAS TO DELIVER ALERT: %d \n", alert.type_string().c_str());
    // TODO: find something better to do here
    buffers->alert_produced = true;
    if ( alert.type() == Botan::TLS::Alert::CLOSE_NOTIFY)
    {
        buffers -> peer_closed_transport = true;
    }
    else if (alert.is_valid() && alert.is_fatal() )
    {
        std::cout << alert.type_string() << std::endl;
        printf("Ignored a fatal alert!!\n");
    } else {
        std::cout << alert.type_string() << std::endl;
        printf("Ignored a non-fatal alert!!\n");
    }
}

bool handshake_cb(buffers_t* buffers, const Botan::TLS::Session&)
{
    //iocba_handshake_cb(botan_pad_ref);
    //printf("botan: HandshakeCompleted\n");
    // TODO: Implement cache management
    buffers -> handshake_completed = true;

    // Pick a protocol if none is available yet
    if ( buffers -> chosen_protocol == NOCHOSENYET_CHP )
    {
        buffers -> chosen_protocol = HTTP11_CHP;
    }
    //printf("Handshake completed\n");
    return false;
}

// TODO: We can use stronger ciphers here. For now let's go simple
class HereTLSPolicy: public Botan::TLS::Policy {
public:
    virtual bool acceptable_protocol_version(const Botan::TLS::Protocol_Version& v)
    {
        return v == Botan::TLS::Protocol_Version::TLS_V12;
    }

    virtual std::vector<std::string> allowed_macs() const
    {
        std::vector<std::string> result;
        result.push_back("AEAD");
        result.push_back("SHA-384");
        result.push_back("SHA-256");
        //
        result.push_back("SHA-1");
        return result;
    }

    virtual std::vector<std::string> allowed_ciphers() const
    {
        std::vector<std::string> result;
        result.push_back("ChaCha20Poly1305");
        result.push_back("AES-256/GCM");
        result.push_back("AES-128/GCM");
        result.push_back("Camellia-256/GCM");
        result.push_back("AES-256/OCB(12)");
        result.push_back("AES-256/CCM");
        // For use with old browsers....
        result.push_back("AES-256");
        result.push_back("3DES");
        return result;
    }

    virtual std::vector<std::string> allowed_key_exchange_methods() const
    {
        std::vector<std::string> result;
        result.push_back("ECDH");
        result.push_back("DH");
        result.push_back("RSA");
        result.push_back("EDCHE");
        return result;
    }

    virtual std::vector<std::string> allowed_signature_hashes() const
    {
        std::vector<std::string> result;
        result.push_back("SHA-256");
        return result;
    }

    virtual std::vector<std::string> allowed_signature_methods() const
    {
        std::vector<std::string> result;
        result.push_back("RSA");
        return result;
    }

    // This is experimental, may need to change.
    virtual bool server_uses_own_ciphersuite_preferences() const
    {
        return true;
    }

};

// TODO: We can use different certificates if needs come....
class HereCredentialsManager: public Botan::Credentials_Manager {
    std::vector<Botan::X509_Certificate> certs;
    Botan::Private_Key* privkey;
public:
    HereCredentialsManager(
        const char* cert_filename,
        const char* privkey_filename,
        Botan::AutoSeeded_RNG& rng
        )
    {
        // In addition to  the certificate itself, build a chain
        // of certificates
        Botan::DataSource_Stream dss(cert_filename);
        int i = 0;
        while ( not dss.end_of_data() )
        {
            try {
                certs.push_back(Botan::X509_Certificate(dss));
            } catch (Botan::Decoding_Error const& err)
            {
                if ( i == 0)
                {
                    throw;
                } else
                {
                    break;
                }
            }
            i ++;
        }
        privkey = Botan::PKCS8::load_key(
            privkey_filename, rng
        );
    }

    HereCredentialsManager(
        const uint8_t* cert_data,
        uint32_t cert_data_len,
        const uint8_t* privkey_data,
        uint32_t privkey_data_len,
        Botan::AutoSeeded_RNG& rng
        )
    {
        std::string cert_string((char*)cert_data, cert_data_len);
        std::stringstream certs_source(cert_string);
        std::string key_string((char*)privkey_data, privkey_data_len);
        std::stringstream key_source(key_string);
        // In addition to  the certificate itself, build a chain
        // of certificates
        Botan::DataSource_Stream certs_source_(certs_source);
        Botan::DataSource_Stream key_source_(key_source);
        int i = 0;
        while ( not certs_source_.end_of_data() )
        {
            try {
                certs.push_back(Botan::X509_Certificate(certs_source_));
            } catch (Botan::Decoding_Error const& err)
            {
                if ( i == 0)
                {
                    throw;
                } else
                {
                    break;
                }
            }
            i ++;
        }
        privkey = Botan::PKCS8::load_key(
            key_source_, rng
        );
    }

    virtual std::vector<
        Botan::X509_Certificate > cert_chain(
            const std::vector< std::string >&,
            const std::string&,
            const std::string& )
    {
        return certs;
    }

    virtual Botan::Private_Key*  private_key_for(const Botan::X509_Certificate &cert, const std::string & type, const std::string &context)
    {
        return privkey;
    }

    ~HereCredentialsManager()
    {
        delete privkey;
    }
};

std::string defaultProtocolSelector(buffers_t* buffers, std::vector<std::string> const& prots)
{
    std::string pass_to_haskell;
    bool is_first = true;
    for (int i=0; i < prots.size(); i++ )
    {
        if (prots[i]=="h2" && buffers->strategy == PREFER_HTTP2 )
        {
            buffers->chosen_protocol = HTTP2_CHP;
            return prots[i];
        }
        if (prots[i]=="http/1.1" && buffers->strategy == PREFER_HTTP11 )
        {
            buffers->chosen_protocol = HTTP11_CHP;
            return prots[i];
        }
    }
    // Got here, maybe can't choose my favourite 
    for (int i=0; i < prots.size(); i++ )
    {
        if (prots[i]=="http/1.1" )
        {
            buffers->chosen_protocol = HTTP11_CHP;
            return prots[i];
        }
        if (prots[i]=="h2" )
        {
            buffers->chosen_protocol = HTTP2_CHP;
            return prots[i];
        }
    }

    // Fallback
    return "http/1.1" ;
}

} // namespace

using namespace second_transfer;

// Called with encrypted data received from the peer. 
extern "C" DLL_PUBLIC int32_t iocba_receive_data(
    buffers_t* buffers,
    char* in_data,
    uint32_t in_length,
    char* out_enc_to_send,
    uint32_t *enc_to_send_length,

    char* out_cleartext_received,
    uint32_t *cleartext_received_length
    )
{
    //printf("Entering iocba_receive_data\n");
    Botan::TLS::Channel* channel = buffers -> channel;
    //
    buffers -> enc_cursor = out_enc_to_send;
    buffers -> enc_end    = out_enc_to_send + *enc_to_send_length;
    //
    buffers -> clr_cursor = out_cleartext_received;
    buffers -> clr_end    = out_cleartext_received + *cleartext_received_length;
    //printf("Botan receives data\n");
    try {
        size_t more_data_required =
            channel->received_data( (const unsigned char*) in_data, in_length);
        //printf("More data required %d \n", more_data_required);
        //printf("After taking data=%p \n", channel);
    }
    catch (buffer_override_exception_t const& e)
    {
        printf("Buffer override!!\n");
        return -1;
    }
    catch (Botan::TLS::TLS_Exception const& e)
    {
        if (e.type() == Botan::TLS::Alert::INAPPROPRIATE_FALLBACK)
        {
            printf("BotanTLS engine instance likely crashed before: %s \n", e.what());
            return -1;
        } else
        {
            printf("BotanTLS engine instance crashed (normal if ALPN didn't go well): %s \n", e.what());
            return -1;
        }
    }
    catch (std::exception const& e)
    {
        // TODO: control messages
        printf("BotanTLS engine crashed with generic exception: %s \n", e.what());
        return -1;
    }
    *enc_to_send_length = (uint32_t) (
        buffers -> enc_cursor 
        - out_enc_to_send );
    //printf("Returning %d bytes \n", *enc_to_send_length);
    *cleartext_received_length = (uint32_t) (
        buffers -> clr_cursor 
        - out_cleartext_received 
        );
    //printf("Returning %d bytes of cleartext \n", *cleartext_received_length);
    // So that we get a clean segfault if we do something wrong
    buffers-> clear_cursors();
    //printf("Clearing cursors and returning\n");
    return 0;
}

extern "C" DLL_PUBLIC int32_t iocba_maybe_get_protocol(buffers_t* buffer)
{
    return (int32_t) buffer->chosen_protocol;
}

extern "C" DLL_PUBLIC int32_t iocba_handshake_completed(buffers_t* buffer)
{
    return (int32_t) buffer->handshake_completed;
}

extern "C" DLL_PUBLIC int32_t iocba_peer_closed_transport(buffers_t* buffer)
{
    return (int32_t) (buffer->peer_closed_transport);
}

// Called with cleartext data we want to encrypt and send back... 
extern "C" DLL_PUBLIC int32_t iocba_cleartext_push(
    buffers_t* buffers,
    char* in_clr,
    uint32_t clr_length,
    char* out_enc_to_send,
    uint32_t *enc_to_send_length
    )
{
    Botan::TLS::Channel* channel = buffers -> channel;
    //
    buffers -> enc_cursor = out_enc_to_send;
    buffers -> enc_end    = out_enc_to_send + *enc_to_send_length;
    try {
        // printf("Before send channel=%p \n", channel);
        channel->send( (const unsigned char*) in_clr, clr_length);
    } catch (buffer_override_exception_t const& )
    {
        printf("Buffer override trying to encrypt cleartext\n");
    }
    catch (std::exception const& e)
    {
        using namespace std;
        cout << "BotanTLS engine raised exception on send: " << e.what() << endl;
        // Clear the buffers, they shall not be used again
        buffers -> clear_cursors();
        return -1;
    }
    *enc_to_send_length = (uint32_t) (
        buffers -> enc_cursor
        - out_enc_to_send );
    buffers -> clear_cursors();
    return 0;
}



extern "C" DLL_PUBLIC void iocba_close(
    buffers_t* buffers,
    char* out_enc_to_send,
    uint32_t *enc_to_send_length
    )
{
    Botan::TLS::Channel* channel = buffers->channel;
    buffers -> enc_cursor = out_enc_to_send;
    buffers -> enc_end    = out_enc_to_send + *enc_to_send_length;
    // No waiting cleartext data in this context
    buffers -> clr_cursor = 0;
    buffers -> clr_end    = 0;
    try{
        channel->close();
    } 
    catch (buffer_override_exception_t const& e)
    {
        printf("Buffer override!!\n");
        return ;
    }
    catch (Botan::TLS::TLS_Exception const& e)
    {
        if (e.type() == Botan::TLS::Alert::INAPPROPRIATE_FALLBACK)
        {
            printf("BotanTLS engine instance likely crashed before: %s \n", e.what());
            return ;
        } else
        {
            printf("BotanTLS engine instance crashed (normal if ALPN didn't go well): %s \n", e.what());
            return ;
        }
    }
    catch (std::exception const& e)
    {
        // TODO: control messages
        printf("BotanTLS engine crashed with generic exception: %s \n", e.what());
        return ;
    }catch (...)
    {
        printf("BotanTLS engine raised exception on close\n");
    }
    *enc_to_send_length = (uint32_t) (
        buffers -> enc_cursor
        - out_enc_to_send );
    buffers -> clear_cursors();
}


struct botan_tls_context_t {
    second_transfer::HereCredentialsManager credentials_manager;
    Botan::TLS::Session_Manager_In_Memory* session_manager;
    Botan::AutoSeeded_RNG* rng;
    std::vector<std::string> protocols;
    second_transfer::HereTLSPolicy here_tls_policty;
    protocol_strategy_enum_t protocol_strategy;
};


extern "C" DLL_PUBLIC botan_tls_context_t* iocba_make_tls_context(
    const char* cert_filename,
    const char* privkey_filename,
    int32_t prt_strt
    )
{
    Botan::AutoSeeded_RNG* rng=new Botan::AutoSeeded_RNG();
    std::vector< std::string > protocols;

    // Not sure what is this good for....
    protocols.push_back("h2");
    protocols.push_back("http/1.1");

    return new botan_tls_context_t{
        second_transfer::HereCredentialsManager(cert_filename, privkey_filename, *rng),
        new Botan::TLS::Session_Manager_In_Memory(*rng),
        rng,
        protocols,
        second_transfer::HereTLSPolicy(),
        (protocol_strategy_enum_t) prt_strt
    };
}


// Oh Gods, forgive me
pthread_mutex_t new_ctx_mutex = PTHREAD_MUTEX_INITIALIZER ;
pthread_mutex_t new_channel_mutex = PTHREAD_MUTEX_INITIALIZER ;

extern "C" DLL_PUBLIC botan_tls_context_t* iocba_make_tls_context_from_memory(
    const uint8_t* cert_data,
    uint32_t cert_data_length,
    const uint8_t* key_data,
    uint32_t key_data_length,
    int32_t prt_strt
)
{
    // printf("New tls context from memory\n");
    pthread_mutex_lock(&new_ctx_mutex);
    Botan::AutoSeeded_RNG* rng=new Botan::AutoSeeded_RNG();
    std::vector< std::string > protocols;

    // Not sure what is this good for....
    protocols.push_back("h2");
    protocols.push_back("http/1.1");

    botan_tls_context_t* result = new botan_tls_context_t{
        second_transfer::HereCredentialsManager(
            cert_data,
            cert_data_length,
            key_data,
            key_data_length,
            *rng),
        new Botan::TLS::Session_Manager_In_Memory(*rng),
        rng,
        protocols,
        second_transfer::HereTLSPolicy(),
        (protocol_strategy_enum_t) prt_strt
    };
    pthread_mutex_unlock(&new_ctx_mutex);
    return result;
}


extern "C" DLL_PUBLIC  void iocba_delete_tls_context(botan_tls_context_t* ctx)
{
    delete ctx->session_manager;
    delete ctx->rng;
    delete ctx;
}


extern "C" DLL_PUBLIC buffers_t* iocba_new_tls_server_channel (
       botan_tls_context_t* ctx)
{
    //printf("New tls server channel\n");
    //pthread_mutex_lock(&new_channel_mutex);
    buffers_t* buffers = new buffers_t(ctx->protocol_strategy);
    auto* server =
        new Botan::TLS::Server(
            std::bind(
                second_transfer::output_dn_cb, 
                buffers, 
                std::placeholders::_1, 
                std::placeholders::_2),
            std::bind(
                second_transfer::data_cb, 
                buffers, 
                std::placeholders::_1, 
                std::placeholders::_2),
            std::bind(
                second_transfer::alert_cb, 
                buffers, 
                std::placeholders::_1, 
                std::placeholders::_2, 
                std::placeholders::_3),
            std::bind(
                second_transfer::handshake_cb, 
                buffers, 
                std::placeholders::_1),
            *(ctx->session_manager),
            ctx->credentials_manager,
            ctx->here_tls_policty,
            *(ctx->rng),
            std::bind(
                second_transfer::defaultProtocolSelector, 
                buffers, 
                std::placeholders::_1)
        );
    buffers->channel = server;
    
    //pthread_mutex_unlock(&new_channel_mutex);
    return buffers;
}

extern "C" DLL_PUBLIC void iocba_delete_tls_server_channel (
    buffers_t* bf
    )
{
    delete bf;
}
