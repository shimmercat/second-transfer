
#ifdef INCLUDE_BOTAN_ALL_H
#include "botan_all.h"
#else

#include <string>
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

#include "Botan_stub.h"
#include <functional>

#include <cstdlib>

// TODO:
//
// Consider marking these functions "safe" in Haskell code.
// For that to work, you need first to change them in such a way that no lock
// takes place, that is, you shouldn't call back to Haskell code from here, but
// instead have the callbacks to simply write that in a provided buffer.
//
// This is also a good point to write about the "TLS overhead."

namespace second_transfer {

// Just because of the conversions, but it may be a handy place for
// other stuff later.
void output_dn_cb (void* botan_pad_ref, const unsigned char a[], size_t sz)
{
    iocba_push(botan_pad_ref, (char*)a, sz);
}

void data_cb (void* botan_pad_ref, const unsigned char a[], size_t sz)
{
    iocba_data_cb(botan_pad_ref, (char*)a, sz);
}

void alert_cb (void* botan_pad_ref, Botan::TLS::Alert const& alert, const unsigned char a[], size_t sz) 
{
    // printf("BOTAN WAS TO DELIVER ALERT: %d \n", alert.type_string().c_str());
    if (alert.is_valid() && alert.is_fatal() )
    {
        // TODO: Propagate this softly.
        iocba_alert_cb(botan_pad_ref, -1);
    } else {
        // printf("Ignore an alert!!\n");
    }
}

bool handshake_cb(void* botan_pad_ref, const Botan::TLS::Session&)
{
    iocba_handshake_cb(botan_pad_ref);
    // TODO: Implement cache management
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

std::string defaultProtocolSelector(void* botan_pad_ref, std::vector<std::string> const& prots)
{
    std::string pass_to_haskell;
    bool is_first = true;
    for (int i=0; i < prots.size(); i++ )
    {
        if ( is_first )
        {
            is_first=false;
        } else
        {
            pass_to_haskell += '\0';
        }
        pass_to_haskell += prots[i];
        //printf("Prot: %s \n", prots[i].c_str());
    }
    int idx = iocba_select_protocol_cb( botan_pad_ref, (void*)pass_to_haskell.c_str(), pass_to_haskell.size());
    //printf("Prot selected: %d \n", idx);
    if ( idx >= 0 )
        return prots[idx];
    else
        throw Botan::TLS::TLS_Exception( Botan::TLS::Alert::NO_APPLICATION_PROTOCOL, "ShimmerCat:NoApplicationProtocol" );
}

} // namespace

extern "C" int iocba_receive_data(
    void* tls_channel,
    char* data,
    int length )
{
    Botan::TLS::Channel* channel = (Botan::TLS::Channel*) tls_channel;
    try {
        //printf("Before taking data=%p \n", channel);
        size_t more_data_required = channel->received_data( (const unsigned char*) data, length);
        //printf("More data required %d \n", more_data_required);
        //printf("After taking data=%p \n", channel);
    } catch (std::exception const& e)
    {
        // TODO: control messages
        printf("BotanTLS engine instance crashed (normal if ALPN didn't go well): %s \n", e.what());
        return -1;
    }
    return 0;
}

extern "C" void iocba_cleartext_push(
    void* tls_channel,
    char* data,
    int length )
{
    // TODO: Check for "can send"!!!!!
    // OTHERWISE THIS WON'T WORK
    try {
        Botan::TLS::Channel* channel = (Botan::TLS::Channel*) tls_channel;
        // printf("Before send channel=%p \n", channel);
        channel->send( (const unsigned char*) data, length);
        // printf("After send channel=%p \n", channel);
    } catch (...)
    {
        printf("BotanTLS engine raised exception on send\n");
    }
}


extern "C" void iocba_close(
    void* tls_channel
    )
{
    Botan::TLS::Channel* channel = (Botan::TLS::Channel*) tls_channel;
    try{
        channel->close();
    } catch (...)
    {
        printf("BotanTLS engine raised exception on close\n");
    }
}


struct botan_tls_context_t {
    second_transfer::HereCredentialsManager credentials_manager;
    Botan::TLS::Session_Manager_In_Memory* session_manager;
    Botan::AutoSeeded_RNG* rng;
    std::vector<std::string> protocols;
    second_transfer::HereTLSPolicy here_tls_policty;
};

extern "C" botan_tls_context_t* iocba_make_tls_context(
    const char* cert_filename,
    const char* privkey_filename
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
        second_transfer::HereTLSPolicy()
    };
}

// Oh Gods, forgive me
pthread_mutex_t new_ctx_mutex = PTHREAD_MUTEX_INITIALIZER ;
pthread_mutex_t new_channel_mutex = PTHREAD_MUTEX_INITIALIZER ;

extern "C" botan_tls_context_t* iocba_make_tls_context_from_memory(
    const uint8_t* cert_data,
    uint32_t cert_data_length,
    const uint8_t* key_data,
    uint32_t key_data_length
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
        second_transfer::HereTLSPolicy()
    };
    pthread_mutex_unlock(&new_ctx_mutex);
    return result;
}


extern "C" void iocba_delete_tls_context(botan_tls_context_t* ctx)
{
    delete ctx->session_manager;
    delete ctx->rng;
    delete ctx;
}


extern "C" void* iocba_new_tls_server_channel (
       void* botan_pad_ref,
       botan_tls_context_t* ctx)
{
    //printf("New tls server channel\n");
    pthread_mutex_lock(&new_channel_mutex);
    auto* server =
        new Botan::TLS::Server(
            std::bind(second_transfer::output_dn_cb, botan_pad_ref, std::placeholders::_1, std::placeholders::_2),
            std::bind(second_transfer::data_cb, botan_pad_ref, std::placeholders::_1, std::placeholders::_2),
            std::bind(second_transfer::alert_cb, botan_pad_ref, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3),
            std::bind(second_transfer::handshake_cb, botan_pad_ref, std::placeholders::_1),
            *(ctx->session_manager),
            ctx->credentials_manager,
            ctx->here_tls_policty,
            *(ctx->rng),
            std::bind(second_transfer::defaultProtocolSelector, botan_pad_ref, std::placeholders::_1)
        );
    pthread_mutex_unlock(&new_channel_mutex);
    return server;
}

extern "C" void iocba_delete_tls_server_channel (
    Botan::TLS::Server * srv
    )
{
    delete srv;
}
