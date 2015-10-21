
#include <functional>

#ifdef INCLUDE_BOTAN_ALL_H
#include "botan_all.h"
#else
#include <botan/botan.h>
#include <botan/tls_session.h>
#include <botan/tls_alert.h>
#include <botan/tls_policy.h>
#include <botan/credentials_manager.h>
#include <botan/tls_channel.h>
#include <botan/pkcs8.h>
#include <botan/tls_session_manager.h>
#include <botan/tls_server.h>
#endif

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
        iocba_alert_cb(botan_pad_ref, -1);
    } // Else: don't care
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
        return result;
    }

    virtual std::vector<std::string> allowed_key_exchange_methods() const
    {
        std::vector<std::string> result;
        result.push_back("ECDH");
        result.push_back("DH");
        result.push_back("RSA");
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
    Botan::X509_Certificate cert;
    std::vector<Botan::X509_Certificate> certs;
    Botan::Private_Key* privkey;
public:
    HereCredentialsManager(
        const char* cert_filename,
        const char* privkey_filename,
        Botan::AutoSeeded_RNG& rng
        ):
            cert(cert_filename)
    {
        certs.push_back(cert);
        privkey = Botan::PKCS8::load_key(
            privkey_filename, rng
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
    }
    int idx = iocba_select_protocol_cb( botan_pad_ref, (void*)pass_to_haskell.c_str(), pass_to_haskell.size());
    return prots[idx];
}

} // namespace

extern "C" void iocba_receive_data(
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
    second_transfer::HereTLSPolicy here_tls_policty;
};

extern "C" botan_tls_context_t* iocba_make_tls_context(
    const char* cert_filename,
    const char* privkey_filename
    )
{
    Botan::AutoSeeded_RNG rng;
    std::vector< std::string > protocols;

    // Not sure what is this good for....
    protocols.push_back("h2");
    protocols.push_back("http/1.1");

    return new botan_tls_context_t{
        second_transfer::HereCredentialsManager(cert_filename, privkey_filename, rng), 
        new Botan::TLS::Session_Manager_In_Memory(rng),
        new Botan::AutoSeeded_RNG(),
        protocols,
        second_transfer::HereTLSPolicy()
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
            ctx->here_tls_policty,
            *(ctx->rng),
            std::bind(second_transfer::defaultProtocolSelector, botan_pad_ref, std::placeholders::_1)
        );
    return server;
}

extern "C" void iocba_delete_tls_server_channel (
    Botan::TLS::Server * srv
    )
{
    delete srv;
}
