
#include <functional>
#include "botan_all.h"

#include "../SecondTransfer/TLS/Botan_stub.h"

#include <cstdlib>

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
    if (alert.is_valid() && alert.is_fatal() )
    {
        // TODO: Propagate this softly.
        printf("ALERT FATAL");
        iocba_alert_cb(botan_pad_ref, -1);
    } // Else: don't care
}

bool handshake_cb(void* botan_pad_ref, const Botan::TLS::Session&)
{
    iocba_handshake_cb(botan_pad_ref);
    // TODO: Implement cache management
    return false;
}

class HereTLSPolicy: public Botan::TLS::Policy {
public:
    virtual bool acceptable_protocol_version(Botan::TLS::Protocol_Version const& v) 
    {
        return v == Botan::TLS::Protocol_Version::TLS_V12;
    }
};

// TODO: We can use different certificates if needs come....
class HereCredentialsManager: public Botan::Credentials_Manager {
    Botan::X509_Certificate cert;
    Botan::Private_Key* privkey;
public:
    HereCredentialsManager(
        const char* cert_filename,
        const char* privkey_filename,
        Botan::AutoSeeded_RNG& rng
        ):
            cert(cert_filename)
    {
        privkey = Botan::PKCS8::load_key(
            privkey_filename, rng);
    }
    virtual std::vector<
        Botan::X509_Certificate > cert_chain(
            const std::vector< std::string >&,
            const std::string&,
            const std::string& )
    {
        
    }

    ~HereCredentialsManager()
    {
        delete privkey;
    }
};

std::string defaultProtocolSelector(std::vector<std::string> const& prots) 
{
    for (int i=0; i < prots.size(); i++ )
    {
        printf( "Prot offered: %s \n", prots[i].c_str() );
        if ( prots[i] == "h2" )
        {
            return prots[i];
        }
    } 
    printf("Defaulting to protocol: %s \n", prots[0].c_str() );
    return prots[0];
}

} // namespace

extern "C" void botan_receive_data(
    void* tls_channel,
    char* data,
    int length )
{
    Botan::TLS::Channel* channel = (Botan::TLS::Channel*) tls_channel;
    channel->received_data( (const unsigned char*) data, length);
}

extern "C" void iocba_cleartext_push(
    void* tls_channel, 
    char* data,
    int length )
{
    // TODO: Check for "can send"!!!!!
    // OTHERWISE THIS WON'T WORK
    Botan::TLS::Channel* channel = (Botan::TLS::Channel*) tls_channel;
    channel->send( (const unsigned char*) data, length);
}


struct botan_tls_context_t {
    second_transfer::HereCredentialsManager credentials_manager;
    Botan::TLS::Session_Manager_In_Memory* session_manager;
    Botan::AutoSeeded_RNG* rng;
    std::vector<std::string> protocols;
};

extern "C" botan_tls_context_t* iocba_make_tls_context(
    const char* cert_filename,
    const char* privkey_filename
    )
{
    Botan::AutoSeeded_RNG rng;
    std::vector< std::string > protocols;
    protocols.push_back("h2");
    protocols.push_back("http/1.1");
    
    return new botan_tls_context_t{
        second_transfer::HereCredentialsManager(cert_filename, privkey_filename, rng), 
        new Botan::TLS::Session_Manager_In_Memory(rng),
        new Botan::AutoSeeded_RNG(),
        protocols
    };
}

extern "C" void iocba_delete_tls_context(botan_tls_context_t* ctx)
{
    delete ctx->session_manager;
    delete ctx->rng;
    delete ctx;
}


extern "C" void* iocba_new_tls_server_channel (
       void* botan_pad_ref, 
       botan_tls_context_t* ctx,
       int protocol_select)
{
    auto* server = 
        new Botan::TLS::Server(
            std::bind(second_transfer::output_dn_cb, botan_pad_ref, std::placeholders::_1, std::placeholders::_2),
            std::bind(second_transfer::data_cb, botan_pad_ref, std::placeholders::_1, std::placeholders::_2),
            std::bind(second_transfer::alert_cb, botan_pad_ref, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3),
            std::bind(second_transfer::handshake_cb, botan_pad_ref, std::placeholders::_1),
            *(ctx->session_manager),
            ctx->credentials_manager,
            second_transfer::HereTLSPolicy(),
            *(ctx->rng),
            &second_transfer::defaultProtocolSelector
        );
    return server;
}

extern "C" void iocba_delete_tls_server_channel (
    Botan::TLS::Server * srv
    )
{
    delete srv;
}
