
#ifndef BOTAN_AMALGAMATION_INTERNAL_H__
#define BOTAN_AMALGAMATION_INTERNAL_H__

#include <algorithm>
#include <chrono>
#include <condition_variable>
#include <deque>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>


#if defined(_MSC_VER) && (_MSC_VER <= 1800)

   #define BOTAN_WORKAROUND_GH_321
   #define NOMINMAX 1
   #define WIN32_LEAN_AND_MEAN 1
   #include <windows.h>

#endif

namespace Botan {

#if defined(BOTAN_WORKAROUND_GH_321)

class WinCS_Mutex
   {
   public:
      WinCS_Mutex()
         {
         ::InitializeCriticalSection(&m_cs);
         }

      ~WinCS_Mutex()
         {
         ::DeleteCriticalSection(&m_cs);
         }

      void lock()
         {
         ::EnterCriticalSection(&m_cs);
         }

      void unlock()
         {
         ::LeaveCriticalSection(&m_cs);
         }

   private:
      CRITICAL_SECTION m_cs;
   };

#endif

template<typename T>
class Algo_Registry
   {
   public:
      typedef typename T::Spec Spec;

      typedef std::function<T* (const Spec&)> maker_fn;

      static Algo_Registry<T>& global_registry()
         {
         static Algo_Registry<T> g_registry;
         return g_registry;
         }

      void add(const std::string& name, const std::string& provider, maker_fn fn, byte pref)
         {
         std::lock_guard<mutex> lock(m_mutex);
         if(!m_algo_info[name].add_provider(provider, fn, pref))
            throw Exception("Duplicated registration of " + name + "/" + provider);
         }

      std::vector<std::string> providers_of(const Spec& spec)
         {
         std::lock_guard<mutex> lock(m_mutex);
         auto i = m_algo_info.find(spec.algo_name());
         if(i != m_algo_info.end())
            return i->second.providers();
         return std::vector<std::string>();
         }

      void set_provider_preference(const Spec& spec, const std::string& provider, byte pref)
         {
         std::lock_guard<mutex> lock(m_mutex);
         auto i = m_algo_info.find(spec.algo_name());
         if(i != m_algo_info.end())
            i->second.set_pref(provider, pref);
         }

      T* make(const Spec& spec, const std::string& provider = "")
         {
         const std::vector<maker_fn> makers = get_makers(spec, provider);

         try
            {
            for(auto&& maker : makers)
               {
               if(T* t = maker(spec))
                  return t;
               }
            }
         catch(std::exception& e)
            {
            throw Lookup_Error("Creating '" + spec.as_string() + "' failed: " + e.what());
            }

         return nullptr;
         }

      class Add
         {
         public:
            Add(const std::string& basename, maker_fn fn, const std::string& provider, byte pref)
               {
               Algo_Registry<T>::global_registry().add(basename, provider, fn, pref);
               }

            Add(bool cond, const std::string& basename, maker_fn fn, const std::string& provider, byte pref)
               {
               if(cond)
                  Algo_Registry<T>::global_registry().add(basename, provider, fn, pref);
               }
         };

   private:

#if defined(BOTAN_WORKAROUND_GH_321)
      using mutex = WinCS_Mutex;
#else
      using mutex = std::mutex;
#endif

      Algo_Registry()  { }

      std::vector<maker_fn> get_makers(const Spec& spec, const std::string& provider)
         {
         std::lock_guard<mutex> lock(m_mutex);
         return m_algo_info[spec.algo_name()].get_makers(provider);
         }

      struct Algo_Info
         {
         public:
            bool add_provider(const std::string& provider, maker_fn fn, byte pref)
               {
               if(m_maker_fns.count(provider) > 0)
                  return false;

               m_maker_fns[provider] = fn;
               m_prefs.insert(std::make_pair(pref, provider));
               return true;
               }

            std::vector<std::string> providers() const
               {
               std::vector<std::string> v;
               for(auto&& k : m_prefs)
                  v.push_back(k.second);
               return v;
               }

            void set_pref(const std::string& provider, byte pref)
               {
               auto i = m_prefs.begin();
               while(i != m_prefs.end())
                  {
                  if(i->second == provider)
                     i = m_prefs.erase(i);
                  else
                     ++i;
                  }
               m_prefs.insert(std::make_pair(pref, provider));
               }

            std::vector<maker_fn> get_makers(const std::string& req_provider)
               {
               std::vector<maker_fn> r;

               if(!req_provider.empty())
                  {
                  // find one explicit provider requested by user or fail
                  auto i = m_maker_fns.find(req_provider);
                  if(i != m_maker_fns.end())
                     r.push_back(i->second);
                  }
               else
                  {
                  for(auto&& pref : m_prefs)
                     r.push_back(m_maker_fns[pref.second]);
                  }

               return r;
               }
         private:
            std::multimap<byte, std::string, std::greater<byte>> m_prefs;
            std::unordered_map<std::string, maker_fn> m_maker_fns;
         };

      mutex m_mutex;
      std::unordered_map<std::string, Algo_Info> m_algo_info;
   };

template<typename T> T*
make_a(const typename T::Spec& spec, const std::string& provider = "")
   {
   return Algo_Registry<T>::global_registry().make(spec, provider);
   }

template<typename T> std::vector<std::string> providers_of(const typename T::Spec& spec)
   {
   return Algo_Registry<T>::global_registry().providers_of(spec);
   }

template<typename T> T*
make_new_T(const typename Algo_Registry<T>::Spec& spec)
   {
   if(spec.arg_count() == 0)
      return new T;
   return nullptr;
   }

template<typename T, size_t DEF_VAL> T*
make_new_T_1len(const typename Algo_Registry<T>::Spec& spec)
   {
   return new T(spec.arg_as_integer(0, DEF_VAL));
   }

template<typename T, size_t DEF1, size_t DEF2> T*
make_new_T_2len(const typename Algo_Registry<T>::Spec& spec)
   {
   return new T(spec.arg_as_integer(0, DEF1), spec.arg_as_integer(1, DEF2));
   }

template<typename T> T*
make_new_T_1str(const typename Algo_Registry<T>::Spec& spec, const std::string& def)
   {
   return new T(spec.arg(0, def));
   }

template<typename T> T*
make_new_T_1str_req(const typename Algo_Registry<T>::Spec& spec)
   {
   return new T(spec.arg(0));
   }

template<typename T, typename X> T*
make_new_T_1X(const typename Algo_Registry<T>::Spec& spec)
   {
   std::unique_ptr<X> x(Algo_Registry<X>::global_registry().make(Botan::SCAN_Name(spec.arg(0))));
   if(!x)
      throw Exception(spec.arg(0));
   return new T(x.release());
   }

#define BOTAN_REGISTER_TYPE(T, type, name, maker, provider, pref)        \
   namespace { Algo_Registry<T>::Add g_ ## type ## _reg(name, maker, provider, pref); } \
   BOTAN_FORCE_SEMICOLON

#define BOTAN_REGISTER_TYPE_COND(cond, T, type, name, maker, provider, pref) \
   namespace { Algo_Registry<T>::Add g_ ## type ## _reg(cond, name, maker, provider, pref); } \
   BOTAN_FORCE_SEMICOLON

#define BOTAN_DEFAULT_ALGORITHM_PRIO 100
#define BOTAN_SIMD_ALGORITHM_PRIO    110

#define BOTAN_REGISTER_NAMED_T(T, name, type, maker)                 \
   BOTAN_REGISTER_TYPE(T, type, name, maker, "base", BOTAN_DEFAULT_ALGORITHM_PRIO)

#define BOTAN_REGISTER_T(T, type, maker)                                \
   BOTAN_REGISTER_TYPE(T, type, #type, maker, "base", BOTAN_DEFAULT_ALGORITHM_PRIO)

#define BOTAN_REGISTER_T_NOARGS(T, type) \
   BOTAN_REGISTER_TYPE(T, type, #type, make_new_T<type>, "base", BOTAN_DEFAULT_ALGORITHM_PRIO)
#define BOTAN_REGISTER_T_1LEN(T, type, def) \
   BOTAN_REGISTER_TYPE(T, type, #type, (make_new_T_1len<type,def>), "base", BOTAN_DEFAULT_ALGORITHM_PRIO)

#define BOTAN_REGISTER_NAMED_T_NOARGS(T, type, name, provider) \
   BOTAN_REGISTER_TYPE(T, type, name, make_new_T<type>, provider, BOTAN_DEFAULT_ALGORITHM_PRIO)
#define BOTAN_COND_REGISTER_NAMED_T_NOARGS(cond, T, type, name, provider, pref) \
   BOTAN_REGISTER_TYPE_COND(cond, T, type, name, make_new_T<type>, provider, pref)

#define BOTAN_REGISTER_NAMED_T_2LEN(T, type, name, provider, len1, len2) \
   BOTAN_REGISTER_TYPE(T, type, name, (make_new_T_2len<type,len1,len2>), provider, BOTAN_DEFAULT_ALGORITHM_PRIO)

}


namespace Botan {

/**
* Power of 2 test. T should be an unsigned integer type
* @param arg an integer value
* @return true iff arg is 2^n for some n > 0
*/
template<typename T>
inline bool is_power_of_2(T arg)
   {
   return ((arg != 0 && arg != 1) && ((arg & (arg-1)) == 0));
   }

/**
* Return the index of the highest set bit
* T is an unsigned integer type
* @param n an integer value
* @return index of the highest set bit in n
*/
template<typename T>
inline size_t high_bit(T n)
   {
   for(size_t i = 8*sizeof(T); i > 0; --i)
      if((n >> (i - 1)) & 0x01)
         return i;
   return 0;
   }

/**
* Return the index of the lowest set bit
* T is an unsigned integer type
* @param n an integer value
* @return index of the lowest set bit in n
*/
template<typename T>
inline size_t low_bit(T n)
   {
   for(size_t i = 0; i != 8*sizeof(T); ++i)
      if((n >> i) & 0x01)
         return (i + 1);
   return 0;
   }

/**
* Return the number of significant bytes in n
* @param n an integer value
* @return number of significant bytes in n
*/
template<typename T>
inline size_t significant_bytes(T n)
   {
   for(size_t i = 0; i != sizeof(T); ++i)
      if(get_byte(i, n))
         return sizeof(T)-i;
   return 0;
   }

/**
* Compute Hamming weights
* @param n an integer value
* @return number of bits in n set to 1
*/
template<typename T>
inline size_t hamming_weight(T n)
   {
   const byte NIBBLE_WEIGHTS[] = {
      0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4 };

   size_t weight = 0;
   for(size_t i = 0; i != 2*sizeof(T); ++i)
      weight += NIBBLE_WEIGHTS[(n >> (4*i)) & 0x0F];
   return weight;
   }

/**
* Count the trailing zero bits in n
* @param n an integer value
* @return maximum x st 2^x divides n
*/
template<typename T>
inline size_t ctz(T n)
   {
   for(size_t i = 0; i != 8*sizeof(T); ++i)
      if((n >> i) & 0x01)
         return i;
   return 8*sizeof(T);
   }

template<typename T>
size_t ceil_log2(T x)
   {
   if(x >> (sizeof(T)*8-1))
      return sizeof(T)*8;

   size_t result = 0;
   T compare = 1;

   while(compare < x)
      {
      compare <<= 1;
      result++;
      }

   return result;
   }

}


namespace Botan {

const u32bit CAST_SBOX1[256] = {
   0x30FB40D4, 0x9FA0FF0B, 0x6BECCD2F, 0x3F258C7A, 0x1E213F2F, 0x9C004DD3,
   0x6003E540, 0xCF9FC949, 0xBFD4AF27, 0x88BBBDB5, 0xE2034090, 0x98D09675,
   0x6E63A0E0, 0x15C361D2, 0xC2E7661D, 0x22D4FF8E, 0x28683B6F, 0xC07FD059,
   0xFF2379C8, 0x775F50E2, 0x43C340D3, 0xDF2F8656, 0x887CA41A, 0xA2D2BD2D,
   0xA1C9E0D6, 0x346C4819, 0x61B76D87, 0x22540F2F, 0x2ABE32E1, 0xAA54166B,
   0x22568E3A, 0xA2D341D0, 0x66DB40C8, 0xA784392F, 0x004DFF2F, 0x2DB9D2DE,
   0x97943FAC, 0x4A97C1D8, 0x527644B7, 0xB5F437A7, 0xB82CBAEF, 0xD751D159,
   0x6FF7F0ED, 0x5A097A1F, 0x827B68D0, 0x90ECF52E, 0x22B0C054, 0xBC8E5935,
   0x4B6D2F7F, 0x50BB64A2, 0xD2664910, 0xBEE5812D, 0xB7332290, 0xE93B159F,
   0xB48EE411, 0x4BFF345D, 0xFD45C240, 0xAD31973F, 0xC4F6D02E, 0x55FC8165,
   0xD5B1CAAD, 0xA1AC2DAE, 0xA2D4B76D, 0xC19B0C50, 0x882240F2, 0x0C6E4F38,
   0xA4E4BFD7, 0x4F5BA272, 0x564C1D2F, 0xC59C5319, 0xB949E354, 0xB04669FE,
   0xB1B6AB8A, 0xC71358DD, 0x6385C545, 0x110F935D, 0x57538AD5, 0x6A390493,
   0xE63D37E0, 0x2A54F6B3, 0x3A787D5F, 0x6276A0B5, 0x19A6FCDF, 0x7A42206A,
   0x29F9D4D5, 0xF61B1891, 0xBB72275E, 0xAA508167, 0x38901091, 0xC6B505EB,
   0x84C7CB8C, 0x2AD75A0F, 0x874A1427, 0xA2D1936B, 0x2AD286AF, 0xAA56D291,
   0xD7894360, 0x425C750D, 0x93B39E26, 0x187184C9, 0x6C00B32D, 0x73E2BB14,
   0xA0BEBC3C, 0x54623779, 0x64459EAB, 0x3F328B82, 0x7718CF82, 0x59A2CEA6,
   0x04EE002E, 0x89FE78E6, 0x3FAB0950, 0x325FF6C2, 0x81383F05, 0x6963C5C8,
   0x76CB5AD6, 0xD49974C9, 0xCA180DCF, 0x380782D5, 0xC7FA5CF6, 0x8AC31511,
   0x35E79E13, 0x47DA91D0, 0xF40F9086, 0xA7E2419E, 0x31366241, 0x051EF495,
   0xAA573B04, 0x4A805D8D, 0x548300D0, 0x00322A3C, 0xBF64CDDF, 0xBA57A68E,
   0x75C6372B, 0x50AFD341, 0xA7C13275, 0x915A0BF5, 0x6B54BFAB, 0x2B0B1426,
   0xAB4CC9D7, 0x449CCD82, 0xF7FBF265, 0xAB85C5F3, 0x1B55DB94, 0xAAD4E324,
   0xCFA4BD3F, 0x2DEAA3E2, 0x9E204D02, 0xC8BD25AC, 0xEADF55B3, 0xD5BD9E98,
   0xE31231B2, 0x2AD5AD6C, 0x954329DE, 0xADBE4528, 0xD8710F69, 0xAA51C90F,
   0xAA786BF6, 0x22513F1E, 0xAA51A79B, 0x2AD344CC, 0x7B5A41F0, 0xD37CFBAD,
   0x1B069505, 0x41ECE491, 0xB4C332E6, 0x032268D4, 0xC9600ACC, 0xCE387E6D,
   0xBF6BB16C, 0x6A70FB78, 0x0D03D9C9, 0xD4DF39DE, 0xE01063DA, 0x4736F464,
   0x5AD328D8, 0xB347CC96, 0x75BB0FC3, 0x98511BFB, 0x4FFBCC35, 0xB58BCF6A,
   0xE11F0ABC, 0xBFC5FE4A, 0xA70AEC10, 0xAC39570A, 0x3F04442F, 0x6188B153,
   0xE0397A2E, 0x5727CB79, 0x9CEB418F, 0x1CACD68D, 0x2AD37C96, 0x0175CB9D,
   0xC69DFF09, 0xC75B65F0, 0xD9DB40D8, 0xEC0E7779, 0x4744EAD4, 0xB11C3274,
   0xDD24CB9E, 0x7E1C54BD, 0xF01144F9, 0xD2240EB1, 0x9675B3FD, 0xA3AC3755,
   0xD47C27AF, 0x51C85F4D, 0x56907596, 0xA5BB15E6, 0x580304F0, 0xCA042CF1,
   0x011A37EA, 0x8DBFAADB, 0x35BA3E4A, 0x3526FFA0, 0xC37B4D09, 0xBC306ED9,
   0x98A52666, 0x5648F725, 0xFF5E569D, 0x0CED63D0, 0x7C63B2CF, 0x700B45E1,
   0xD5EA50F1, 0x85A92872, 0xAF1FBDA7, 0xD4234870, 0xA7870BF3, 0x2D3B4D79,
   0x42E04198, 0x0CD0EDE7, 0x26470DB8, 0xF881814C, 0x474D6AD7, 0x7C0C5E5C,
   0xD1231959, 0x381B7298, 0xF5D2F4DB, 0xAB838653, 0x6E2F1E23, 0x83719C9E,
   0xBD91E046, 0x9A56456E, 0xDC39200C, 0x20C8C571, 0x962BDA1C, 0xE1E696FF,
   0xB141AB08, 0x7CCA89B9, 0x1A69E783, 0x02CC4843, 0xA2F7C579, 0x429EF47D,
   0x427B169C, 0x5AC9F049, 0xDD8F0F00, 0x5C8165BF };

const u32bit CAST_SBOX2[256] = {
   0x1F201094, 0xEF0BA75B, 0x69E3CF7E, 0x393F4380, 0xFE61CF7A, 0xEEC5207A,
   0x55889C94, 0x72FC0651, 0xADA7EF79, 0x4E1D7235, 0xD55A63CE, 0xDE0436BA,
   0x99C430EF, 0x5F0C0794, 0x18DCDB7D, 0xA1D6EFF3, 0xA0B52F7B, 0x59E83605,
   0xEE15B094, 0xE9FFD909, 0xDC440086, 0xEF944459, 0xBA83CCB3, 0xE0C3CDFB,
   0xD1DA4181, 0x3B092AB1, 0xF997F1C1, 0xA5E6CF7B, 0x01420DDB, 0xE4E7EF5B,
   0x25A1FF41, 0xE180F806, 0x1FC41080, 0x179BEE7A, 0xD37AC6A9, 0xFE5830A4,
   0x98DE8B7F, 0x77E83F4E, 0x79929269, 0x24FA9F7B, 0xE113C85B, 0xACC40083,
   0xD7503525, 0xF7EA615F, 0x62143154, 0x0D554B63, 0x5D681121, 0xC866C359,
   0x3D63CF73, 0xCEE234C0, 0xD4D87E87, 0x5C672B21, 0x071F6181, 0x39F7627F,
   0x361E3084, 0xE4EB573B, 0x602F64A4, 0xD63ACD9C, 0x1BBC4635, 0x9E81032D,
   0x2701F50C, 0x99847AB4, 0xA0E3DF79, 0xBA6CF38C, 0x10843094, 0x2537A95E,
   0xF46F6FFE, 0xA1FF3B1F, 0x208CFB6A, 0x8F458C74, 0xD9E0A227, 0x4EC73A34,
   0xFC884F69, 0x3E4DE8DF, 0xEF0E0088, 0x3559648D, 0x8A45388C, 0x1D804366,
   0x721D9BFD, 0xA58684BB, 0xE8256333, 0x844E8212, 0x128D8098, 0xFED33FB4,
   0xCE280AE1, 0x27E19BA5, 0xD5A6C252, 0xE49754BD, 0xC5D655DD, 0xEB667064,
   0x77840B4D, 0xA1B6A801, 0x84DB26A9, 0xE0B56714, 0x21F043B7, 0xE5D05860,
   0x54F03084, 0x066FF472, 0xA31AA153, 0xDADC4755, 0xB5625DBF, 0x68561BE6,
   0x83CA6B94, 0x2D6ED23B, 0xECCF01DB, 0xA6D3D0BA, 0xB6803D5C, 0xAF77A709,
   0x33B4A34C, 0x397BC8D6, 0x5EE22B95, 0x5F0E5304, 0x81ED6F61, 0x20E74364,
   0xB45E1378, 0xDE18639B, 0x881CA122, 0xB96726D1, 0x8049A7E8, 0x22B7DA7B,
   0x5E552D25, 0x5272D237, 0x79D2951C, 0xC60D894C, 0x488CB402, 0x1BA4FE5B,
   0xA4B09F6B, 0x1CA815CF, 0xA20C3005, 0x8871DF63, 0xB9DE2FCB, 0x0CC6C9E9,
   0x0BEEFF53, 0xE3214517, 0xB4542835, 0x9F63293C, 0xEE41E729, 0x6E1D2D7C,
   0x50045286, 0x1E6685F3, 0xF33401C6, 0x30A22C95, 0x31A70850, 0x60930F13,
   0x73F98417, 0xA1269859, 0xEC645C44, 0x52C877A9, 0xCDFF33A6, 0xA02B1741,
   0x7CBAD9A2, 0x2180036F, 0x50D99C08, 0xCB3F4861, 0xC26BD765, 0x64A3F6AB,
   0x80342676, 0x25A75E7B, 0xE4E6D1FC, 0x20C710E6, 0xCDF0B680, 0x17844D3B,
   0x31EEF84D, 0x7E0824E4, 0x2CCB49EB, 0x846A3BAE, 0x8FF77888, 0xEE5D60F6,
   0x7AF75673, 0x2FDD5CDB, 0xA11631C1, 0x30F66F43, 0xB3FAEC54, 0x157FD7FA,
   0xEF8579CC, 0xD152DE58, 0xDB2FFD5E, 0x8F32CE19, 0x306AF97A, 0x02F03EF8,
   0x99319AD5, 0xC242FA0F, 0xA7E3EBB0, 0xC68E4906, 0xB8DA230C, 0x80823028,
   0xDCDEF3C8, 0xD35FB171, 0x088A1BC8, 0xBEC0C560, 0x61A3C9E8, 0xBCA8F54D,
   0xC72FEFFA, 0x22822E99, 0x82C570B4, 0xD8D94E89, 0x8B1C34BC, 0x301E16E6,
   0x273BE979, 0xB0FFEAA6, 0x61D9B8C6, 0x00B24869, 0xB7FFCE3F, 0x08DC283B,
   0x43DAF65A, 0xF7E19798, 0x7619B72F, 0x8F1C9BA4, 0xDC8637A0, 0x16A7D3B1,
   0x9FC393B7, 0xA7136EEB, 0xC6BCC63E, 0x1A513742, 0xEF6828BC, 0x520365D6,
   0x2D6A77AB, 0x3527ED4B, 0x821FD216, 0x095C6E2E, 0xDB92F2FB, 0x5EEA29CB,
   0x145892F5, 0x91584F7F, 0x5483697B, 0x2667A8CC, 0x85196048, 0x8C4BACEA,
   0x833860D4, 0x0D23E0F9, 0x6C387E8A, 0x0AE6D249, 0xB284600C, 0xD835731D,
   0xDCB1C647, 0xAC4C56EA, 0x3EBD81B3, 0x230EABB0, 0x6438BC87, 0xF0B5B1FA,
   0x8F5EA2B3, 0xFC184642, 0x0A036B7A, 0x4FB089BD, 0x649DA589, 0xA345415E,
   0x5C038323, 0x3E5D3BB9, 0x43D79572, 0x7E6DD07C, 0x06DFDF1E, 0x6C6CC4EF,
   0x7160A539, 0x73BFBE70, 0x83877605, 0x4523ECF1 };

const u32bit CAST_SBOX3[256] = {
   0x8DEFC240, 0x25FA5D9F, 0xEB903DBF, 0xE810C907, 0x47607FFF, 0x369FE44B,
   0x8C1FC644, 0xAECECA90, 0xBEB1F9BF, 0xEEFBCAEA, 0xE8CF1950, 0x51DF07AE,
   0x920E8806, 0xF0AD0548, 0xE13C8D83, 0x927010D5, 0x11107D9F, 0x07647DB9,
   0xB2E3E4D4, 0x3D4F285E, 0xB9AFA820, 0xFADE82E0, 0xA067268B, 0x8272792E,
   0x553FB2C0, 0x489AE22B, 0xD4EF9794, 0x125E3FBC, 0x21FFFCEE, 0x825B1BFD,
   0x9255C5ED, 0x1257A240, 0x4E1A8302, 0xBAE07FFF, 0x528246E7, 0x8E57140E,
   0x3373F7BF, 0x8C9F8188, 0xA6FC4EE8, 0xC982B5A5, 0xA8C01DB7, 0x579FC264,
   0x67094F31, 0xF2BD3F5F, 0x40FFF7C1, 0x1FB78DFC, 0x8E6BD2C1, 0x437BE59B,
   0x99B03DBF, 0xB5DBC64B, 0x638DC0E6, 0x55819D99, 0xA197C81C, 0x4A012D6E,
   0xC5884A28, 0xCCC36F71, 0xB843C213, 0x6C0743F1, 0x8309893C, 0x0FEDDD5F,
   0x2F7FE850, 0xD7C07F7E, 0x02507FBF, 0x5AFB9A04, 0xA747D2D0, 0x1651192E,
   0xAF70BF3E, 0x58C31380, 0x5F98302E, 0x727CC3C4, 0x0A0FB402, 0x0F7FEF82,
   0x8C96FDAD, 0x5D2C2AAE, 0x8EE99A49, 0x50DA88B8, 0x8427F4A0, 0x1EAC5790,
   0x796FB449, 0x8252DC15, 0xEFBD7D9B, 0xA672597D, 0xADA840D8, 0x45F54504,
   0xFA5D7403, 0xE83EC305, 0x4F91751A, 0x925669C2, 0x23EFE941, 0xA903F12E,
   0x60270DF2, 0x0276E4B6, 0x94FD6574, 0x927985B2, 0x8276DBCB, 0x02778176,
   0xF8AF918D, 0x4E48F79E, 0x8F616DDF, 0xE29D840E, 0x842F7D83, 0x340CE5C8,
   0x96BBB682, 0x93B4B148, 0xEF303CAB, 0x984FAF28, 0x779FAF9B, 0x92DC560D,
   0x224D1E20, 0x8437AA88, 0x7D29DC96, 0x2756D3DC, 0x8B907CEE, 0xB51FD240,
   0xE7C07CE3, 0xE566B4A1, 0xC3E9615E, 0x3CF8209D, 0x6094D1E3, 0xCD9CA341,
   0x5C76460E, 0x00EA983B, 0xD4D67881, 0xFD47572C, 0xF76CEDD9, 0xBDA8229C,
   0x127DADAA, 0x438A074E, 0x1F97C090, 0x081BDB8A, 0x93A07EBE, 0xB938CA15,
   0x97B03CFF, 0x3DC2C0F8, 0x8D1AB2EC, 0x64380E51, 0x68CC7BFB, 0xD90F2788,
   0x12490181, 0x5DE5FFD4, 0xDD7EF86A, 0x76A2E214, 0xB9A40368, 0x925D958F,
   0x4B39FFFA, 0xBA39AEE9, 0xA4FFD30B, 0xFAF7933B, 0x6D498623, 0x193CBCFA,
   0x27627545, 0x825CF47A, 0x61BD8BA0, 0xD11E42D1, 0xCEAD04F4, 0x127EA392,
   0x10428DB7, 0x8272A972, 0x9270C4A8, 0x127DE50B, 0x285BA1C8, 0x3C62F44F,
   0x35C0EAA5, 0xE805D231, 0x428929FB, 0xB4FCDF82, 0x4FB66A53, 0x0E7DC15B,
   0x1F081FAB, 0x108618AE, 0xFCFD086D, 0xF9FF2889, 0x694BCC11, 0x236A5CAE,
   0x12DECA4D, 0x2C3F8CC5, 0xD2D02DFE, 0xF8EF5896, 0xE4CF52DA, 0x95155B67,
   0x494A488C, 0xB9B6A80C, 0x5C8F82BC, 0x89D36B45, 0x3A609437, 0xEC00C9A9,
   0x44715253, 0x0A874B49, 0xD773BC40, 0x7C34671C, 0x02717EF6, 0x4FEB5536,
   0xA2D02FFF, 0xD2BF60C4, 0xD43F03C0, 0x50B4EF6D, 0x07478CD1, 0x006E1888,
   0xA2E53F55, 0xB9E6D4BC, 0xA2048016, 0x97573833, 0xD7207D67, 0xDE0F8F3D,
   0x72F87B33, 0xABCC4F33, 0x7688C55D, 0x7B00A6B0, 0x947B0001, 0x570075D2,
   0xF9BB88F8, 0x8942019E, 0x4264A5FF, 0x856302E0, 0x72DBD92B, 0xEE971B69,
   0x6EA22FDE, 0x5F08AE2B, 0xAF7A616D, 0xE5C98767, 0xCF1FEBD2, 0x61EFC8C2,
   0xF1AC2571, 0xCC8239C2, 0x67214CB8, 0xB1E583D1, 0xB7DC3E62, 0x7F10BDCE,
   0xF90A5C38, 0x0FF0443D, 0x606E6DC6, 0x60543A49, 0x5727C148, 0x2BE98A1D,
   0x8AB41738, 0x20E1BE24, 0xAF96DA0F, 0x68458425, 0x99833BE5, 0x600D457D,
   0x282F9350, 0x8334B362, 0xD91D1120, 0x2B6D8DA0, 0x642B1E31, 0x9C305A00,
   0x52BCE688, 0x1B03588A, 0xF7BAEFD5, 0x4142ED9C, 0xA4315C11, 0x83323EC5,
   0xDFEF4636, 0xA133C501, 0xE9D3531C, 0xEE353783 };

const u32bit CAST_SBOX4[256] = {
   0x9DB30420, 0x1FB6E9DE, 0xA7BE7BEF, 0xD273A298, 0x4A4F7BDB, 0x64AD8C57,
   0x85510443, 0xFA020ED1, 0x7E287AFF, 0xE60FB663, 0x095F35A1, 0x79EBF120,
   0xFD059D43, 0x6497B7B1, 0xF3641F63, 0x241E4ADF, 0x28147F5F, 0x4FA2B8CD,
   0xC9430040, 0x0CC32220, 0xFDD30B30, 0xC0A5374F, 0x1D2D00D9, 0x24147B15,
   0xEE4D111A, 0x0FCA5167, 0x71FF904C, 0x2D195FFE, 0x1A05645F, 0x0C13FEFE,
   0x081B08CA, 0x05170121, 0x80530100, 0xE83E5EFE, 0xAC9AF4F8, 0x7FE72701,
   0xD2B8EE5F, 0x06DF4261, 0xBB9E9B8A, 0x7293EA25, 0xCE84FFDF, 0xF5718801,
   0x3DD64B04, 0xA26F263B, 0x7ED48400, 0x547EEBE6, 0x446D4CA0, 0x6CF3D6F5,
   0x2649ABDF, 0xAEA0C7F5, 0x36338CC1, 0x503F7E93, 0xD3772061, 0x11B638E1,
   0x72500E03, 0xF80EB2BB, 0xABE0502E, 0xEC8D77DE, 0x57971E81, 0xE14F6746,
   0xC9335400, 0x6920318F, 0x081DBB99, 0xFFC304A5, 0x4D351805, 0x7F3D5CE3,
   0xA6C866C6, 0x5D5BCCA9, 0xDAEC6FEA, 0x9F926F91, 0x9F46222F, 0x3991467D,
   0xA5BF6D8E, 0x1143C44F, 0x43958302, 0xD0214EEB, 0x022083B8, 0x3FB6180C,
   0x18F8931E, 0x281658E6, 0x26486E3E, 0x8BD78A70, 0x7477E4C1, 0xB506E07C,
   0xF32D0A25, 0x79098B02, 0xE4EABB81, 0x28123B23, 0x69DEAD38, 0x1574CA16,
   0xDF871B62, 0x211C40B7, 0xA51A9EF9, 0x0014377B, 0x041E8AC8, 0x09114003,
   0xBD59E4D2, 0xE3D156D5, 0x4FE876D5, 0x2F91A340, 0x557BE8DE, 0x00EAE4A7,
   0x0CE5C2EC, 0x4DB4BBA6, 0xE756BDFF, 0xDD3369AC, 0xEC17B035, 0x06572327,
   0x99AFC8B0, 0x56C8C391, 0x6B65811C, 0x5E146119, 0x6E85CB75, 0xBE07C002,
   0xC2325577, 0x893FF4EC, 0x5BBFC92D, 0xD0EC3B25, 0xB7801AB7, 0x8D6D3B24,
   0x20C763EF, 0xC366A5FC, 0x9C382880, 0x0ACE3205, 0xAAC9548A, 0xECA1D7C7,
   0x041AFA32, 0x1D16625A, 0x6701902C, 0x9B757A54, 0x31D477F7, 0x9126B031,
   0x36CC6FDB, 0xC70B8B46, 0xD9E66A48, 0x56E55A79, 0x026A4CEB, 0x52437EFF,
   0x2F8F76B4, 0x0DF980A5, 0x8674CDE3, 0xEDDA04EB, 0x17A9BE04, 0x2C18F4DF,
   0xB7747F9D, 0xAB2AF7B4, 0xEFC34D20, 0x2E096B7C, 0x1741A254, 0xE5B6A035,
   0x213D42F6, 0x2C1C7C26, 0x61C2F50F, 0x6552DAF9, 0xD2C231F8, 0x25130F69,
   0xD8167FA2, 0x0418F2C8, 0x001A96A6, 0x0D1526AB, 0x63315C21, 0x5E0A72EC,
   0x49BAFEFD, 0x187908D9, 0x8D0DBD86, 0x311170A7, 0x3E9B640C, 0xCC3E10D7,
   0xD5CAD3B6, 0x0CAEC388, 0xF73001E1, 0x6C728AFF, 0x71EAE2A1, 0x1F9AF36E,
   0xCFCBD12F, 0xC1DE8417, 0xAC07BE6B, 0xCB44A1D8, 0x8B9B0F56, 0x013988C3,
   0xB1C52FCA, 0xB4BE31CD, 0xD8782806, 0x12A3A4E2, 0x6F7DE532, 0x58FD7EB6,
   0xD01EE900, 0x24ADFFC2, 0xF4990FC5, 0x9711AAC5, 0x001D7B95, 0x82E5E7D2,
   0x109873F6, 0x00613096, 0xC32D9521, 0xADA121FF, 0x29908415, 0x7FBB977F,
   0xAF9EB3DB, 0x29C9ED2A, 0x5CE2A465, 0xA730F32C, 0xD0AA3FE8, 0x8A5CC091,
   0xD49E2CE7, 0x0CE454A9, 0xD60ACD86, 0x015F1919, 0x77079103, 0xDEA03AF6,
   0x78A8565E, 0xDEE356DF, 0x21F05CBE, 0x8B75E387, 0xB3C50651, 0xB8A5C3EF,
   0xD8EEB6D2, 0xE523BE77, 0xC2154529, 0x2F69EFDF, 0xAFE67AFB, 0xF470C4B2,
   0xF3E0EB5B, 0xD6CC9876, 0x39E4460C, 0x1FDA8538, 0x1987832F, 0xCA007367,
   0xA99144F8, 0x296B299E, 0x492FC295, 0x9266BEAB, 0xB5676E69, 0x9BD3DDDA,
   0xDF7E052F, 0xDB25701C, 0x1B5E51EE, 0xF65324E6, 0x6AFCE36C, 0x0316CC04,
   0x8644213E, 0xB7DC59D0, 0x7965291F, 0xCCD6FD43, 0x41823979, 0x932BCDF6,
   0xB657C34D, 0x4EDFD282, 0x7AE5290C, 0x3CB9536B, 0x851E20FE, 0x9833557E,
   0x13ECF0B0, 0xD3FFB372, 0x3F85C5C1, 0x0AEF7ED2 };

}


namespace Botan {

void gcm_multiply_clmul(byte x[16], const byte H[16]);

}


namespace Botan {

/**
* Expand an input to a bit mask depending on it being being zero or non-zero
* @ param tst the input
* @return the mask 0xFFFF if tst is non-zero and 0 otherwise
*/
template<typename T>
u16bit expand_mask_16bit(T tst)
   {
   const u16bit result = (tst != 0);
   return ~(result - 1);
   }

inline gf2m gray_to_lex(gf2m gray)
   {
   gf2m result = gray ^ (gray >> 8);
   result ^= (result >> 4);
   result ^= (result >> 2);
   result ^= (result >> 1);
   return result;
   }

inline gf2m lex_to_gray(gf2m lex)
   {
   return (lex >> 1) ^ lex;
   }

inline u32bit bit_size_to_byte_size(u32bit bit_size)
   {
   return (bit_size - 1) / 8 + 1;
   }

inline u32bit bit_size_to_32bit_size(u32bit bit_size)
   {
   return (bit_size - 1) / 32 + 1;
   }

}


namespace Botan {

/*
* Allocation Size Tracking Helper for Zlib/Bzlib/LZMA
*/
class Compression_Alloc_Info
   {
   public:
      template<typename T>
      static void* malloc(void* self, T n, T size)
         {
         return static_cast<Compression_Alloc_Info*>(self)->do_malloc(n, size);
         }

      static void free(void* self, void* ptr)
         {
         static_cast<Compression_Alloc_Info*>(self)->do_free(ptr);
         }

   private:
      void* do_malloc(size_t n, size_t size);
      void do_free(void* ptr);

      std::unordered_map<void*, size_t> m_current_allocs;
   };

/**
* Wrapper for Zlib/Bzlib/LZMA stream types
*/
template<typename Stream, typename ByteType>
class Zlib_Style_Stream : public Compression_Stream
   {
   public:
      void next_in(byte* b, size_t len) override
         {
         m_stream.next_in = reinterpret_cast<ByteType*>(b);
         m_stream.avail_in = len;
         }

      void next_out(byte* b, size_t len) override
         {
         m_stream.next_out = reinterpret_cast<ByteType*>(b);
         m_stream.avail_out = len;
         }

      size_t avail_in() const override { return m_stream.avail_in; }

      size_t avail_out() const override { return m_stream.avail_out; }

      Zlib_Style_Stream()
         {
         clear_mem(&m_stream, 1);
         m_allocs.reset(new Compression_Alloc_Info);
         }

      ~Zlib_Style_Stream()
         {
         clear_mem(&m_stream, 1);
         m_allocs.reset();
         }

   protected:
      typedef Stream stream_t;

      stream_t* streamp() { return &m_stream; }

      Compression_Alloc_Info* alloc() { return m_allocs.get(); }
   private:
      stream_t m_stream;
      std::unique_ptr<Compression_Alloc_Info> m_allocs;
   };

#define BOTAN_REGISTER_COMPRESSION(C, D) \
   BOTAN_REGISTER_T_NOARGS(Compression_Algorithm, C); \
   BOTAN_REGISTER_T_NOARGS(Decompression_Algorithm, D)

}


#if defined(BOTAN_HAS_VALGRIND)
  #include <valgrind/memcheck.h>
#endif

namespace Botan {

namespace CT {

/**
* Use valgrind to mark the contents of memory as being undefined.
* Valgrind will accept operations which manipulate undefined values,
* but will warn if an undefined value is used to decided a conditional
* jump or a load/store address. So if we poison all of our inputs we
* can confirm that the operations in question are truly const time
* when compiled by whatever compiler is in use.
*
* Even better, the VALGRIND_MAKE_MEM_* macros work even when the
* program is not run under valgrind (though with a few cycles of
* overhead, which is unfortunate in final binaries as these
* annotations tend to be used in fairly important loops).
*
* This approach was first used in ctgrind (https://github.com/agl/ctgrind)
* but calling the valgrind mecheck API directly works just as well and
* doesn't require a custom patched valgrind.
*/
template<typename T>
inline void poison(const T* p, size_t n)
   {
#if defined(BOTAN_HAS_VALGRIND)
   VALGRIND_MAKE_MEM_UNDEFINED(p, n * sizeof(T));
#else
   BOTAN_UNUSED(p);
   BOTAN_UNUSED(n);
#endif
   }

template<typename T>
inline void unpoison(const T* p, size_t n)
   {
#if defined(BOTAN_HAS_VALGRIND)
   VALGRIND_MAKE_MEM_DEFINED(p, n * sizeof(T));
#else
   BOTAN_UNUSED(p);
   BOTAN_UNUSED(n);
#endif
   }

template<typename T>
inline void unpoison(T& p)
   {
#if defined(BOTAN_HAS_VALGRIND)
   VALGRIND_MAKE_MEM_DEFINED(&p, sizeof(T));
#else
   BOTAN_UNUSED(p);
#endif
   }

/*
* T should be an unsigned machine integer type
* Expand to a mask used for other operations
* @param in an integer
* @return If n is zero, returns zero. Otherwise
* returns a T with all bits set for use as a mask with
* select.
*/
template<typename T>
inline T expand_mask(T x)
   {
   T r = x;
   // First fold r down to a single bit
   for(size_t i = 1; i != sizeof(T)*8; i *= 2)
      r |= r >> i;
   r &= 1;
   r = ~(r - 1);
   return r;
   }

template<typename T>
inline T select(T mask, T from0, T from1)
   {
   return (from0 & mask) | (from1 & ~mask);
   }

template<typename PredT, typename ValT>
inline ValT val_or_zero(PredT pred_val, ValT val)
   {
   return select(CT::expand_mask<ValT>(pred_val), val, static_cast<ValT>(0));
   }

template<typename T>
inline T is_zero(T x)
   {
   return ~expand_mask(x);
   }

template<typename T>
inline T is_equal(T x, T y)
   {
   return is_zero(x ^ y);
   }

template<typename T>
inline T is_less(T x, T y)
   {
   /*
   This expands to a constant time sequence with GCC 5.2.0 on x86-64
   but something more complicated may be needed for portable const time.
   */
   return expand_mask<T>(x < y);
   }

template<typename T>
inline T is_lte(T x, T y)
   {
   return expand_mask<T>(x <= y);
   }

template<typename T>
inline void conditional_copy_mem(T value,
                                 T* to,
                                 const T* from0,
                                 const T* from1,
                                 size_t elems)
   {
   const T mask = CT::expand_mask(value);

   for(size_t i = 0; i != elems; ++i)
      {
      to[i] = CT::select(mask, from0[i], from1[i]);
      }
   }

template<typename T>
inline void cond_zero_mem(T cond,
                          T* array,
                          size_t elems)
   {
   const T mask = CT::expand_mask(cond);
   const T zero(0);

   for(size_t i = 0; i != elems; ++i)
      {
      array[i] = CT::select(mask, zero, array[i]);
      }
   }

template<typename T>
inline T expand_top_bit(T a)
   {
   return expand_mask<T>(a >> (sizeof(T)*8-1));
   }

template<typename T>
inline T max(T a, T b)
   {
   const T a_larger = b - a; // negative if a is larger
   return select(expand_top_bit(a), a, b);
   }

template<typename T>
inline T min(T a, T b)
   {
   const T a_larger = b - a; // negative if a is larger
   return select(expand_top_bit(b), b, a);
   }

inline secure_vector<uint8_t> strip_leading_zeros(const uint8_t in[], size_t length)
   {
   size_t leading_zeros = 0;

   uint8_t only_zeros = 0xFF;

   for(size_t i = 0; i != length; ++i)
      {
      only_zeros &= CT::is_zero(in[i]);
      leading_zeros += CT::select<uint8_t>(only_zeros, 1, 0);
      }

   return secure_vector<byte>(in + leading_zeros, in + length);
   }

inline secure_vector<byte> strip_leading_zeros(const secure_vector<uint8_t>& in)
   {
   return strip_leading_zeros(in.data(), in.size());
   }

}

}


namespace Botan {

/**
* Fixed Window Exponentiator
*/
class Fixed_Window_Exponentiator : public Modular_Exponentiator
   {
   public:
      void set_exponent(const BigInt&) override;
      void set_base(const BigInt&) override;
      BigInt execute() const override;

      Modular_Exponentiator* copy() const override
         { return new Fixed_Window_Exponentiator(*this); }

      Fixed_Window_Exponentiator(const BigInt&, Power_Mod::Usage_Hints);
   private:
      Modular_Reducer m_reducer;
      BigInt m_exp;
      size_t m_window_bits;
      std::vector<BigInt> m_g;
      Power_Mod::Usage_Hints m_hints;
   };

/**
* Montgomery Exponentiator
*/
class Montgomery_Exponentiator : public Modular_Exponentiator
   {
   public:
      void set_exponent(const BigInt&) override;
      void set_base(const BigInt&) override;
      BigInt execute() const override;

      Modular_Exponentiator* copy() const override
         { return new Montgomery_Exponentiator(*this); }

      Montgomery_Exponentiator(const BigInt&, Power_Mod::Usage_Hints);
   private:
      BigInt m_exp, m_modulus, m_R_mod, m_R2_mod;
      word m_mod_prime;
      size_t m_mod_words, m_exp_bits, m_window_bits;
      Power_Mod::Usage_Hints m_hints;
      std::vector<BigInt> m_g;
   };

}


namespace Botan {

/**
* Entropy source reading from kernel devices like /dev/random
*/
class Device_EntropySource final : public Entropy_Source
   {
   public:
      std::string name() const override { return "dev_random"; }

      void poll(Entropy_Accumulator& accum) override;

      Device_EntropySource(const std::vector<std::string>& fsnames);
      ~Device_EntropySource();
   private:
      typedef int fd_type;
      std::vector<fd_type> m_devices;
   };

}


namespace Botan {

class donna128
   {
   public:
      donna128(u64bit ll = 0, u64bit hh = 0) { l = ll; h = hh; }

      donna128(const donna128&) = default;
      donna128& operator=(const donna128&) = default;

      friend donna128 operator>>(const donna128& x, size_t shift)
         {
         donna128 z = x;
         const u64bit carry = z.h << (64 - shift);
         z.h = (z.h >> shift);
         z.l = (z.l >> shift) | carry;
         return z;
         }

      friend donna128 operator<<(const donna128& x, size_t shift)
         {
         donna128 z = x;
         const u64bit carry = z.l >> (64 - shift);
         z.l = (z.l << shift);
         z.h = (z.h << shift) | carry;
         return z;
         }

      friend u64bit operator&(const donna128& x, u64bit mask)
         {
         return x.l & mask;
         }

      u64bit operator&=(u64bit mask)
         {
         h = 0;
         l &= mask;
         return l;
         }

      donna128& operator+=(const donna128& x)
         {
         l += x.l;
         h += (l < x.l);
         h += x.h;
         return *this;
         }

      donna128& operator+=(u64bit x)
         {
         l += x;
         h += (l < x);
         return *this;
         }

      u64bit lo() const { return l; }
      u64bit hi() const { return h; }
   private:
      u64bit h = 0, l = 0;
   };

inline donna128 operator*(const donna128& x, u64bit y)
   {
   BOTAN_ASSERT(x.hi() == 0, "High 64 bits of donna128 set to zero during multiply");

   u64bit lo = 0, hi = 0;
   mul64x64_128(x.lo(), y, &lo, &hi);
   return donna128(lo, hi);
   }

inline donna128 operator+(const donna128& x, const donna128& y)
   {
   donna128 z = x;
   z += y;
   return z;
   }

inline donna128 operator+(const donna128& x, u64bit y)
   {
   donna128 z = x;
   z += y;
   return z;
   }

inline donna128 operator|(const donna128& x, const donna128& y)
   {
   return donna128(x.lo() | y.lo(), x.hi() | y.hi());
   }

inline u64bit carry_shift(const donna128& a, size_t shift)
   {
   return (a >> shift).lo();
   }

inline u64bit combine_lower(const donna128& a, size_t s1,
                            const donna128& b, size_t s2)
   {
   donna128 z = (a >> s1) | (b << s2);
   return z.lo();
   }

#if defined(BOTAN_TARGET_HAS_NATIVE_UINT128)
inline u64bit carry_shift(const uint128_t a, size_t shift)
   {
   return static_cast<u64bit>(a >> shift);
   }

inline u64bit combine_lower(const uint128_t a, size_t s1,
                            const uint128_t b, size_t s2)
   {
   return static_cast<u64bit>((a >> s1) | (b << s2));
   }
#endif

}


namespace Botan {

/**
* EGD Entropy Source
*/
class EGD_EntropySource final : public Entropy_Source
   {
   public:
      std::string name() const override { return "egd"; }

      void poll(Entropy_Accumulator& accum) override;

      EGD_EntropySource(const std::vector<std::string>&);
      ~EGD_EntropySource();
   private:
      class EGD_Socket
         {
         public:
            EGD_Socket(const std::string& path);

            void close();
            size_t read(byte outbuf[], size_t length);
         private:
            static int open_socket(const std::string& path);

            std::string m_socket_path;
            int m_fd; // cached fd
         };

      std::mutex m_mutex;
      std::vector<EGD_Socket> m_sockets;
   };

}


namespace Botan {

BOTAN_DLL std::vector<std::string> get_files_recursive(const std::string& dir);

}


namespace Botan {

/**
* Entropy source using high resolution timers
*
* @note Any results from timers are marked as not contributing entropy
* to the poll, as a local attacker could observe them directly.
*/
class High_Resolution_Timestamp final : public Entropy_Source
   {
   public:
      std::string name() const override { return "timestamp"; }
      void poll(Entropy_Accumulator& accum) override;
   };

}


namespace Botan {

void mceliece_decrypt(secure_vector<byte>& plaintext_out,
                      secure_vector<byte>& error_mask_out,
                      const byte ciphertext[],
                      size_t ciphertext_len,
                      const McEliece_PrivateKey& key);

void mceliece_decrypt(secure_vector<byte>& plaintext_out,
                      secure_vector<byte>& error_mask_out,
                      const secure_vector<byte>& ciphertext,
                      const McEliece_PrivateKey& key);

secure_vector<byte> mceliece_decrypt(
   secure_vector<gf2m> & error_pos,
   const byte *ciphertext, u32bit ciphertext_len,
   const McEliece_PrivateKey & key);

void mceliece_encrypt(secure_vector<byte>& ciphertext_out,
                      secure_vector<byte>& error_mask_out,
                      const secure_vector<byte>& plaintext,
                      const McEliece_PublicKey& key,
                      RandomNumberGenerator& rng);

McEliece_PrivateKey generate_mceliece_key(RandomNumberGenerator &rng,
                                          u32bit ext_deg,
                                          u32bit code_length,
                                          u32bit t);

}



namespace Botan {

/**
* Round up
* @param n a non-negative integer
* @param align_to the alignment boundary
* @return n rounded up to a multiple of align_to
*/
inline size_t round_up(size_t n, size_t align_to)
   {
   BOTAN_ASSERT(align_to != 0, "align_to must not be 0");

   if(n % align_to)
      n += align_to - (n % align_to);
   return n;
   }

/**
* Round down
* @param n an integer
* @param align_to the alignment boundary
* @return n rounded down to a multiple of align_to
*/
template<typename T>
inline T round_down(T n, T align_to)
   {
   if(align_to == 0)
      return n;

   return (n - (n % align_to));
   }

/**
* Clamp
*/
inline size_t clamp(size_t n, size_t lower_bound, size_t upper_bound)
   {
   if(n < lower_bound)
      return lower_bound;
   if(n > upper_bound)
      return upper_bound;
   return n;
   }

}


namespace Botan {

template<typename T>
T* make_block_cipher_mode(const Cipher_Mode::Spec& spec)
   {
   if(std::unique_ptr<BlockCipher> bc = BlockCipher::create(spec.arg(0)))
      return new T(bc.release());
   return nullptr;
   }

template<typename T, size_t LEN1>
T* make_block_cipher_mode_len(const Cipher_Mode::Spec& spec)
   {
   if(std::unique_ptr<BlockCipher> bc = BlockCipher::create(spec.arg(0)))
      {
      const size_t len1 = spec.arg_as_integer(1, LEN1);
      return new T(bc.release(), len1);
      }

   return nullptr;
   }

template<typename T, size_t LEN1, size_t LEN2>
T* make_block_cipher_mode_len2(const Cipher_Mode::Spec& spec)
   {
   if(std::unique_ptr<BlockCipher> bc = BlockCipher::create(spec.arg(0)))
      {
      const size_t len1 = spec.arg_as_integer(1, LEN1);
      const size_t len2 = spec.arg_as_integer(2, LEN2);
      return new T(bc.release(), len1, len2);
      }

   return nullptr;
   }

#define BOTAN_REGISTER_BLOCK_CIPHER_MODE(E, D)                          \
   BOTAN_REGISTER_NAMED_T(Cipher_Mode, #E, E, make_block_cipher_mode<E>); \
   BOTAN_REGISTER_NAMED_T(Cipher_Mode, #D, D, make_block_cipher_mode<D>)

#define BOTAN_REGISTER_BLOCK_CIPHER_MODE_LEN(E, D, LEN)                          \
   BOTAN_REGISTER_NAMED_T(Cipher_Mode, #E, E, (make_block_cipher_mode_len<E, LEN>)); \
   BOTAN_REGISTER_NAMED_T(Cipher_Mode, #D, D, (make_block_cipher_mode_len<D, LEN>))

#define BOTAN_REGISTER_BLOCK_CIPHER_MODE_LEN2(E, D, LEN1, LEN2)                          \
   BOTAN_REGISTER_NAMED_T(Cipher_Mode, #E, E, (make_block_cipher_mode_len2<E, LEN1, LEN2>)); \
   BOTAN_REGISTER_NAMED_T(Cipher_Mode, #D, D, (make_block_cipher_mode_len2<D, LEN1, LEN2>))

}


#if (BOTAN_MP_WORD_BITS != 64)
   #error The mp_x86_64 module requires that BOTAN_MP_WORD_BITS == 64
#endif

namespace Botan {

/*
* Helper Macros for x86-64 Assembly
*/
#define ASM(x) x "\n\t"

/*
* Word Multiply
*/
inline word word_madd2(word a, word b, word* c)
   {
   asm(
      ASM("mulq %[b]")
      ASM("addq %[c],%[a]")
      ASM("adcq $0,%[carry]")

      : [a]"=a"(a), [b]"=rm"(b), [carry]"=&d"(*c)
      : "0"(a), "1"(b), [c]"g"(*c) : "cc");

   return a;
   }

/*
* Word Multiply/Add
*/
inline word word_madd3(word a, word b, word c, word* d)
   {
   asm(
      ASM("mulq %[b]")

      ASM("addq %[c],%[a]")
      ASM("adcq $0,%[carry]")

      ASM("addq %[d],%[a]")
      ASM("adcq $0,%[carry]")

      : [a]"=a"(a), [b]"=rm"(b), [carry]"=&d"(*d)
      : "0"(a), "1"(b), [c]"g"(c), [d]"g"(*d) : "cc");

   return a;
   }

#undef ASM

}


namespace Botan {

/*
* Helper Macros for x86-64 Assembly
*/
#ifndef ASM
  #define ASM(x) x "\n\t"
#endif

#define ADDSUB2_OP(OPERATION, INDEX)                     \
        ASM("movq 8*" #INDEX "(%[y]), %[carry]")         \
        ASM(OPERATION " %[carry], 8*" #INDEX "(%[x])")   \

#define ADDSUB3_OP(OPERATION, INDEX)                     \
        ASM("movq 8*" #INDEX "(%[x]), %[carry]")         \
        ASM(OPERATION " 8*" #INDEX "(%[y]), %[carry]")   \
        ASM("movq %[carry], 8*" #INDEX "(%[z])")         \

#define LINMUL_OP(WRITE_TO, INDEX)                       \
        ASM("movq 8*" #INDEX "(%[x]),%%rax")             \
        ASM("mulq %[y]")                                 \
        ASM("addq %[carry],%%rax")                       \
        ASM("adcq $0,%%rdx")                             \
        ASM("movq %%rdx,%[carry]")                       \
        ASM("movq %%rax, 8*" #INDEX "(%[" WRITE_TO "])")

#define MULADD_OP(IGNORED, INDEX)                        \
        ASM("movq 8*" #INDEX "(%[x]),%%rax")             \
        ASM("mulq %[y]")                                 \
        ASM("addq %[carry],%%rax")                       \
        ASM("adcq $0,%%rdx")                             \
        ASM("addq 8*" #INDEX "(%[z]),%%rax")             \
        ASM("adcq $0,%%rdx")                             \
        ASM("movq %%rdx,%[carry]")                       \
        ASM("movq %%rax, 8*" #INDEX " (%[z])")

#define DO_8_TIMES(MACRO, ARG) \
        MACRO(ARG, 0) \
        MACRO(ARG, 1) \
        MACRO(ARG, 2) \
        MACRO(ARG, 3) \
        MACRO(ARG, 4) \
        MACRO(ARG, 5) \
        MACRO(ARG, 6) \
        MACRO(ARG, 7)

#define ADD_OR_SUBTRACT(CORE_CODE)     \
        ASM("rorq %[carry]")           \
        CORE_CODE                      \
        ASM("sbbq %[carry],%[carry]")  \
        ASM("negq %[carry]")

/*
* Word Addition
*/
inline word word_add(word x, word y, word* carry)
   {
   asm(
      ADD_OR_SUBTRACT(ASM("adcq %[y],%[x]"))
      : [x]"=r"(x), [carry]"=r"(*carry)
      : "0"(x), [y]"rm"(y), "1"(*carry)
      : "cc");
   return x;
   }

/*
* Eight Word Block Addition, Two Argument
*/
inline word word8_add2(word x[8], const word y[8], word carry)
   {
   asm(
      ADD_OR_SUBTRACT(DO_8_TIMES(ADDSUB2_OP, "adcq"))
      : [carry]"=r"(carry)
      : [x]"r"(x), [y]"r"(y), "0"(carry)
      : "cc", "memory");
   return carry;
   }

/*
* Eight Word Block Addition, Three Argument
*/
inline word word8_add3(word z[8], const word x[8], const word y[8], word carry)
   {
   asm(
      ADD_OR_SUBTRACT(DO_8_TIMES(ADDSUB3_OP, "adcq"))
      : [carry]"=r"(carry)
      : [x]"r"(x), [y]"r"(y), [z]"r"(z), "0"(carry)
      : "cc", "memory");
   return carry;
   }

/*
* Word Subtraction
*/
inline word word_sub(word x, word y, word* carry)
   {
   asm(
      ADD_OR_SUBTRACT(ASM("sbbq %[y],%[x]"))
      : [x]"=r"(x), [carry]"=r"(*carry)
      : "0"(x), [y]"rm"(y), "1"(*carry)
      : "cc");
   return x;
   }

/*
* Eight Word Block Subtraction, Two Argument
*/
inline word word8_sub2(word x[8], const word y[8], word carry)
   {
   asm(
      ADD_OR_SUBTRACT(DO_8_TIMES(ADDSUB2_OP, "sbbq"))
      : [carry]"=r"(carry)
      : [x]"r"(x), [y]"r"(y), "0"(carry)
      : "cc", "memory");
   return carry;
   }

/*
* Eight Word Block Subtraction, Two Argument
*/
inline word word8_sub2_rev(word x[8], const word y[8], word carry)
   {
   asm(
      ADD_OR_SUBTRACT(DO_8_TIMES(ADDSUB3_OP, "sbbq"))
      : [carry]"=r"(carry)
      : [x]"r"(y), [y]"r"(x), [z]"r"(x), "0"(carry)
      : "cc", "memory");
   return carry;
   }

/*
* Eight Word Block Subtraction, Three Argument
*/
inline word word8_sub3(word z[8], const word x[8], const word y[8], word carry)
   {
   asm(
      ADD_OR_SUBTRACT(DO_8_TIMES(ADDSUB3_OP, "sbbq"))
      : [carry]"=r"(carry)
      : [x]"r"(x), [y]"r"(y), [z]"r"(z), "0"(carry)
      : "cc", "memory");
   return carry;
   }

/*
* Eight Word Block Linear Multiplication
*/
inline word word8_linmul2(word x[8], word y, word carry)
   {
   asm(
      DO_8_TIMES(LINMUL_OP, "x")
      : [carry]"=r"(carry)
      : [x]"r"(x), [y]"rm"(y), "0"(carry)
      : "cc", "%rax", "%rdx");
   return carry;
   }

/*
* Eight Word Block Linear Multiplication
*/
inline word word8_linmul3(word z[8], const word x[8], word y, word carry)
   {
   asm(
      DO_8_TIMES(LINMUL_OP, "z")
      : [carry]"=r"(carry)
      : [z]"r"(z), [x]"r"(x), [y]"rm"(y), "0"(carry)
      : "cc", "%rax", "%rdx");
   return carry;
   }

/*
* Eight Word Block Multiply/Add
*/
inline word word8_madd3(word z[8], const word x[8], word y, word carry)
   {
   asm(
      DO_8_TIMES(MULADD_OP, "")
      : [carry]"=r"(carry)
      : [z]"r"(z), [x]"r"(x), [y]"rm"(y), "0"(carry)
      : "cc", "%rax", "%rdx");
   return carry;
   }

/*
* Multiply-Add Accumulator
*/
inline void word3_muladd(word* w2, word* w1, word* w0, word x, word y)
   {
   asm(
      ASM("mulq %[y]")

      ASM("addq %[x],%[w0]")
      ASM("adcq %[y],%[w1]")
      ASM("adcq $0,%[w2]")

      : [w0]"=r"(*w0), [w1]"=r"(*w1), [w2]"=r"(*w2)
      : [x]"a"(x), [y]"d"(y), "0"(*w0), "1"(*w1), "2"(*w2)
      : "cc");
   }

/*
* Multiply-Add Accumulator
*/
inline void word3_muladd_2(word* w2, word* w1, word* w0, word x, word y)
   {
   asm(
      ASM("mulq %[y]")

      ASM("addq %[x],%[w0]")
      ASM("adcq %[y],%[w1]")
      ASM("adcq $0,%[w2]")

      ASM("addq %[x],%[w0]")
      ASM("adcq %[y],%[w1]")
      ASM("adcq $0,%[w2]")

      : [w0]"=r"(*w0), [w1]"=r"(*w1), [w2]"=r"(*w2)
      : [x]"a"(x), [y]"d"(y), "0"(*w0), "1"(*w1), "2"(*w2)
      : "cc");
   }

#undef ASM
#undef DO_8_TIMES
#undef ADD_OR_SUBTRACT
#undef ADDSUB2_OP
#undef ADDSUB3_OP
#undef LINMUL_OP
#undef MULADD_OP

}


namespace Botan {

/*
* The size of the word type, in bits
*/
const size_t MP_WORD_BITS = BOTAN_MP_WORD_BITS;

/*
* If cond == 0, does nothing.
* If cond > 0, swaps x[0:size] with y[0:size]
* Runs in constant time
*/
BOTAN_DLL
void bigint_cnd_swap(word cnd, word x[], word y[], size_t size);

/*
* If cond > 0 adds x[0:size] to y[0:size] and returns carry
* Runs in constant time
*/
BOTAN_DLL
word bigint_cnd_add(word cnd, word x[], const word y[], size_t size);

/*
* If cond > 0 subs x[0:size] to y[0:size] and returns borrow
* Runs in constant time
*/
BOTAN_DLL
word bigint_cnd_sub(word cnd, word x[], const word y[], size_t size);

/*
* 2s complement absolute value
* If cond > 0 sets x to ~x + 1
* Runs in constant time
*/
BOTAN_DLL
void bigint_cnd_abs(word cnd, word x[], size_t size);

/**
* Two operand addition
* @param x the first operand (and output)
* @param x_size size of x
* @param y the second operand
* @param y_size size of y (must be >= x_size)
*/
void bigint_add2(word x[], size_t x_size,
                 const word y[], size_t y_size);

/**
* Three operand addition
*/
void bigint_add3(word z[],
                 const word x[], size_t x_size,
                 const word y[], size_t y_size);

/**
* Two operand addition with carry out
*/
word bigint_add2_nc(word x[], size_t x_size, const word y[], size_t y_size);

/**
* Three operand addition with carry out
*/
word bigint_add3_nc(word z[],
                    const word x[], size_t x_size,
                    const word y[], size_t y_size);

/**
* Two operand subtraction
*/
word bigint_sub2(word x[], size_t x_size,
                 const word y[], size_t y_size);

/**
* Two operand subtraction, x = y - x; assumes y >= x
*/
void bigint_sub2_rev(word x[], const word y[], size_t y_size);

/**
* Three operand subtraction
*/
word bigint_sub3(word z[],
                 const word x[], size_t x_size,
                 const word y[], size_t y_size);

/*
* Shift Operations
*/
void bigint_shl1(word x[], size_t x_size,
                 size_t word_shift, size_t bit_shift);

void bigint_shr1(word x[], size_t x_size,
                 size_t word_shift, size_t bit_shift);

void bigint_shl2(word y[], const word x[], size_t x_size,
                 size_t word_shift, size_t bit_shift);

void bigint_shr2(word y[], const word x[], size_t x_size,
                 size_t word_shift, size_t bit_shift);

/*
* Linear Multiply
*/
void bigint_linmul2(word x[], size_t x_size, word y);
void bigint_linmul3(word z[], const word x[], size_t x_size, word y);

/**
* Montgomery Reduction
* @param z integer to reduce, of size exactly 2*(p_size+1).
           Output is in the first p_size+1 words, higher
           words are set to zero.
* @param p modulus
* @param p_size size of p
* @param p_dash Montgomery value
* @param workspace array of at least 2*(p_size+1) words
*/
void bigint_monty_redc(word z[],
                       const word p[], size_t p_size,
                       word p_dash,
                       word workspace[]);

/*
* Montgomery Multiplication
*/
void bigint_monty_mul(word z[], size_t z_size,
                      const word x[], size_t x_size, size_t x_sw,
                      const word y[], size_t y_size, size_t y_sw,
                      const word p[], size_t p_size, word p_dash,
                      word workspace[]);

/*
* Montgomery Squaring
*/
void bigint_monty_sqr(word z[], size_t z_size,
                      const word x[], size_t x_size, size_t x_sw,
                      const word p[], size_t p_size, word p_dash,
                      word workspace[]);

/**
* Compare x and y
*/
s32bit bigint_cmp(const word x[], size_t x_size,
                  const word y[], size_t y_size);

/**
* Compute ((n1<<bits) + n0) / d
*/
word bigint_divop(word n1, word n0, word d);

/**
* Compute ((n1<<bits) + n0) % d
*/
word bigint_modop(word n1, word n0, word d);

/*
* Comba Multiplication / Squaring
*/
void bigint_comba_mul4(word z[8], const word x[4], const word y[4]);
void bigint_comba_mul6(word z[12], const word x[6], const word y[6]);
void bigint_comba_mul8(word z[16], const word x[8], const word y[8]);
void bigint_comba_mul9(word z[18], const word x[9], const word y[9]);
void bigint_comba_mul16(word z[32], const word x[16], const word y[16]);

void bigint_comba_sqr4(word out[8], const word in[4]);
void bigint_comba_sqr6(word out[12], const word in[6]);
void bigint_comba_sqr8(word out[16], const word in[8]);
void bigint_comba_sqr9(word out[18], const word in[9]);
void bigint_comba_sqr16(word out[32], const word in[16]);

/*
* High Level Multiplication/Squaring Interfaces
*/
void bigint_mul(word z[], size_t z_size, word workspace[],
                const word x[], size_t x_size, size_t x_sw,
                const word y[], size_t y_size, size_t y_sw);

void bigint_sqr(word z[], size_t z_size, word workspace[],
                const word x[], size_t x_size, size_t x_sw);

}


namespace Botan {

namespace OS {

/**
* Returns the OS assigned process ID, if available. Otherwise returns 0.
*/
uint32_t get_process_id();

/**
* Returns the value of the hardware cycle counter, if available.
* Returns 0 if not available. On Windows uses QueryPerformanceCounter.
* On other platforms reads the native cycle counter directly.
* The epoch and update rate are arbitrary and may not be constant
* (depending on the hardware).
*/
uint64_t get_processor_timestamp();

/**
* Returns the value of the system clock with best resolution available,
* normalized to nanoseconds resolution.
*/
uint64_t get_system_timestamp_ns();

/*
* Returns the maximum amount of memory (in bytes) we could/should
* hyptothetically allocate. Reads "BOTAN_MLOCK_POOL_SIZE" from
* environment which can be set to zero.
*/
size_t get_memory_locking_limit();

/*
* Request so many bytes of page-aligned RAM locked into memory using
* mlock, VirtualLock, or similar. Returns null on failure. The memory
* returned is zeroed. Free it with free_locked_pages.
*/
void* allocate_locked_pages(size_t length);

/*
* Free memory allocated by allocate_locked_pages
*/
void free_locked_pages(void* ptr, size_t length);

}

}


namespace Botan {

/**
* Container of output buffers for Pipe
*/
class Output_Buffers
   {
   public:
      size_t read(byte[], size_t, Pipe::message_id);
      size_t peek(byte[], size_t, size_t, Pipe::message_id) const;
      size_t get_bytes_read(Pipe::message_id) const;
      size_t remaining(Pipe::message_id) const;

      void add(class SecureQueue*);
      void retire();

      Pipe::message_id message_count() const;

      Output_Buffers();
      ~Output_Buffers();
   private:
      class SecureQueue* get(Pipe::message_id) const;

      std::deque<SecureQueue*> m_buffers;
      Pipe::message_id m_offset;
   };

}


namespace Botan {

Public_Key* make_public_key(const AlgorithmIdentifier& alg_id,
                            const secure_vector<byte>& key_bits);

Private_Key* make_private_key(const AlgorithmIdentifier& alg_id,
                              const secure_vector<byte>& key_bits,
                              RandomNumberGenerator& rng);

}


namespace Botan {

namespace PK_Ops {

class Encryption_with_EME : public Encryption
   {
   public:
      size_t max_input_bits() const override;

      secure_vector<byte> encrypt(const byte msg[], size_t msg_len,
                                  RandomNumberGenerator& rng) override;

      ~Encryption_with_EME();
   protected:
      explicit Encryption_with_EME(const std::string& eme);
   private:
      virtual size_t max_raw_input_bits() const = 0;

      virtual secure_vector<byte> raw_encrypt(const byte msg[], size_t len,
                                              RandomNumberGenerator& rng) = 0;
      std::unique_ptr<EME> m_eme;
   };

class Decryption_with_EME : public Decryption
   {
   public:
      size_t max_input_bits() const override;

      secure_vector<byte> decrypt(byte& valid_mask,
                                  const byte msg[], size_t msg_len) override;

      ~Decryption_with_EME();
   protected:
      explicit Decryption_with_EME(const std::string& eme);
   private:
      virtual size_t max_raw_input_bits() const = 0;
      virtual secure_vector<byte> raw_decrypt(const byte msg[], size_t len) = 0;
      std::unique_ptr<EME> m_eme;
   };

class Verification_with_EMSA : public Verification
   {
   public:
      void update(const byte msg[], size_t msg_len) override;
      bool is_valid_signature(const byte sig[], size_t sig_len) override;

      bool do_check(const secure_vector<byte>& msg,
                    const byte sig[], size_t sig_len);

   protected:

      explicit Verification_with_EMSA(const std::string& emsa);
      ~Verification_with_EMSA();

      /**
      * @return boolean specifying if this key type supports message
      * recovery and thus if you need to call verify() or verify_mr()
      */
      virtual bool with_recovery() const = 0;

      /*
      * Perform a signature check operation
      * @param msg the message
      * @param msg_len the length of msg in bytes
      * @param sig the signature
      * @param sig_len the length of sig in bytes
      * @returns if signature is a valid one for message
      */
      virtual bool verify(const byte[], size_t,
                          const byte[], size_t)
         {
         throw Invalid_State("Message recovery required");
         }

      /*
      * Perform a signature operation (with message recovery)
      * Only call this if with_recovery() returns true
      * @param msg the message
      * @param msg_len the length of msg in bytes
      * @returns recovered message
      */
      virtual secure_vector<byte> verify_mr(const byte[], size_t)
         {
         throw Invalid_State("Message recovery not supported");
         }

   private:
      std::unique_ptr<EMSA> m_emsa;
   };

class Signature_with_EMSA : public Signature
   {
   public:
      void update(const byte msg[], size_t msg_len) override;

      secure_vector<byte> sign(RandomNumberGenerator& rng) override;
   protected:
      explicit Signature_with_EMSA(const std::string& emsa);
      ~Signature_with_EMSA();
   private:

      /**
      * Get the maximum message size in bits supported by this public key.
      * @return maximum message in bits
      */
      virtual size_t max_input_bits() const = 0;

      bool self_test_signature(const std::vector<byte>& msg,
                               const std::vector<byte>& sig) const;

      virtual secure_vector<byte> raw_sign(const byte msg[], size_t msg_len,
                                           RandomNumberGenerator& rng) = 0;

      std::unique_ptr<EMSA> m_emsa;
   };

class Key_Agreement_with_KDF : public Key_Agreement
   {
   public:
      secure_vector<byte> agree(size_t key_len,
                                const byte other_key[], size_t other_key_len,
                                const byte salt[], size_t salt_len) override;

   protected:
      explicit Key_Agreement_with_KDF(const std::string& kdf);
      ~Key_Agreement_with_KDF();
   private:
      virtual secure_vector<byte> raw_agree(const byte w[], size_t w_len) = 0;
      std::unique_ptr<KDF> m_kdf;
   };

class KEM_Encryption_with_KDF : public KEM_Encryption
   {
   public:
      void kem_encrypt(secure_vector<byte>& out_encapsulated_key,
                       secure_vector<byte>& out_shared_key,
                       size_t desired_shared_key_len,
                       Botan::RandomNumberGenerator& rng,
                       const uint8_t salt[],
                       size_t salt_len) override;

   protected:
      virtual void raw_kem_encrypt(secure_vector<byte>& out_encapsulated_key,
                                   secure_vector<byte>& raw_shared_key,
                                   Botan::RandomNumberGenerator& rng) = 0;

      explicit KEM_Encryption_with_KDF(const std::string& kdf);
      ~KEM_Encryption_with_KDF();
   private:
      std::unique_ptr<KDF> m_kdf;
   };

class KEM_Decryption_with_KDF : public KEM_Decryption
   {
   public:
      secure_vector<byte> kem_decrypt(const byte encap_key[],
                                      size_t len,
                                      size_t desired_shared_key_len,
                                      const uint8_t salt[],
                                      size_t salt_len) override;

   protected:
      virtual secure_vector<byte>
      raw_kem_decrypt(const byte encap_key[], size_t len) = 0;

      explicit KEM_Decryption_with_KDF(const std::string& kdf);
      ~KEM_Decryption_with_KDF();
   private:
      std::unique_ptr<KDF> m_kdf;
   };

}

}


namespace Botan {

template<typename OP, typename T>
OP* make_pk_op(const typename T::Spec& spec)
   {
   if(auto* key = dynamic_cast<const typename T::Key_Type*>(&spec.key()))
      return new T(*key, spec.padding());
   return nullptr;
   }

#define BOTAN_REGISTER_PK_OP(T, NAME, TYPE) BOTAN_REGISTER_NAMED_T(T, NAME, TYPE, (make_pk_op<T, TYPE>))

#define BOTAN_REGISTER_PK_ENCRYPTION_OP(NAME, TYPE) BOTAN_REGISTER_PK_OP(PK_Ops::Encryption, NAME, TYPE)
#define BOTAN_REGISTER_PK_DECRYPTION_OP(NAME, TYPE) BOTAN_REGISTER_PK_OP(PK_Ops::Decryption, NAME, TYPE)
#define BOTAN_REGISTER_PK_SIGNATURE_OP(NAME, TYPE) BOTAN_REGISTER_PK_OP(PK_Ops::Signature, NAME, TYPE)
#define BOTAN_REGISTER_PK_VERIFY_OP(NAME, TYPE) BOTAN_REGISTER_PK_OP(PK_Ops::Verification, NAME, TYPE)
#define BOTAN_REGISTER_PK_KEY_AGREE_OP(NAME, TYPE) BOTAN_REGISTER_PK_OP(PK_Ops::Key_Agreement, NAME, TYPE)

#define BOTAN_REGISTER_PK_KEM_ENCRYPTION_OP(NAME, TYPE) BOTAN_REGISTER_PK_OP(PK_Ops::KEM_Encryption, NAME, TYPE)
#define BOTAN_REGISTER_PK_KEM_DECRYPTION_OP(NAME, TYPE) BOTAN_REGISTER_PK_OP(PK_Ops::KEM_Decryption, NAME, TYPE)

}


namespace Botan {

template<typename T>
inline void prefetch_readonly(const T* addr, size_t length)
   {
#if defined(__GNUG__)
   const size_t Ts_per_cache_line = CPUID::cache_line_size() / sizeof(T);

   for(size_t i = 0; i <= length; i += Ts_per_cache_line)
      __builtin_prefetch(addr + i, 0);
#endif
   }

template<typename T>
inline void prefetch_readwrite(const T* addr, size_t length)
   {
#if defined(__GNUG__)
   const size_t Ts_per_cache_line = CPUID::cache_line_size() / sizeof(T);

   for(size_t i = 0; i <= length; i += Ts_per_cache_line)
      __builtin_prefetch(addr + i, 1);
#endif
   }

}


namespace Botan {

class File_Descriptor_Source
   {
   public:
      virtual int next_fd() = 0;
      virtual ~File_Descriptor_Source() {}
   };

/**
* File Tree Walking Entropy Source
*/
class ProcWalking_EntropySource final : public Entropy_Source
   {
   public:
      std::string name() const override { return "proc_walk"; }

      void poll(Entropy_Accumulator& accum) override;

      ProcWalking_EntropySource(const std::string& root_dir) :
         m_path(root_dir), m_dir(nullptr) {}

   private:
      const std::string m_path;
      std::mutex m_mutex;
      std::unique_ptr<File_Descriptor_Source> m_dir;
      secure_vector<byte> m_buf;
   };

}


namespace Botan {

/**
* Entropy source using the rdrand instruction first introduced on
* Intel's Ivy Bridge architecture.
*/
class Intel_Rdrand final : public Entropy_Source
   {
   public:
      std::string name() const override { return "rdrand"; }
      void poll(Entropy_Accumulator& accum) override;
   };

}


namespace Botan {

/**
* Entropy source using the rdseed instruction first introduced on
* Intel's Broadwell architecture.
*/
class Intel_Rdseed final : public Entropy_Source
   {
   public:
      std::string name() const override { return "rdseed"; }
      void poll(Entropy_Accumulator& accum) override;
   };

}


namespace Botan {

class Semaphore
   {
   public:
      explicit Semaphore(int value = 0) : m_value(value), m_wakeups(0) {}

      void acquire();

      void release(size_t n = 1);

   private:
      int m_value;
      int m_wakeups;
      std::mutex m_mutex;
      std::condition_variable m_cond;
   };

}

#define SBoxE1(B0, B1, B2, B3)                    \
   do {                                           \
      B3 ^= B0;                                   \
      auto B4 = B1;                               \
      B1 &= B3;                                   \
      B4 ^= B2;                                   \
      B1 ^= B0;                                   \
      B0 |= B3;                                   \
      B0 ^= B4;                                   \
      B4 ^= B3;                                   \
      B3 ^= B2;                                   \
      B2 |= B1;                                   \
      B2 ^= B4;                                   \
      B4 = ~B4;                                   \
      B4 |= B1;                                   \
      B1 ^= B3;                                   \
      B1 ^= B4;                                   \
      B3 |= B0;                                   \
      B1 ^= B3;                                   \
      B4 ^= B3;                                   \
      B3 = B0;                                    \
      B0 = B1;                                    \
      B1 = B4;                                    \
   } while(0);

#define SBoxE2(B0, B1, B2, B3)                    \
   do {                                           \
      B0 = ~B0;                                   \
      B2 = ~B2;                                   \
      auto B4 = B0;                               \
      B0 &= B1;                                   \
      B2 ^= B0;                                   \
      B0 |= B3;                                   \
      B3 ^= B2;                                   \
      B1 ^= B0;                                   \
      B0 ^= B4;                                   \
      B4 |= B1;                                   \
      B1 ^= B3;                                   \
      B2 |= B0;                                   \
      B2 &= B4;                                   \
      B0 ^= B1;                                   \
      B1 &= B2;                                   \
      B1 ^= B0;                                   \
      B0 &= B2;                                   \
      B4 ^= B0;                                   \
      B0 = B2;                                    \
      B2 = B3;                                    \
      B3 = B1;                                    \
      B1 = B4;                                    \
   } while(0);

#define SBoxE3(B0, B1, B2, B3)                    \
   do {                                           \
      auto B4 = B0;                               \
      B0 &= B2;                                   \
      B0 ^= B3;                                   \
      B2 ^= B1;                                   \
      B2 ^= B0;                                   \
      B3 |= B4;                                   \
      B3 ^= B1;                                   \
      B4 ^= B2;                                   \
      B1 = B3;                                    \
      B3 |= B4;                                   \
      B3 ^= B0;                                   \
      B0 &= B1;                                   \
      B4 ^= B0;                                   \
      B1 ^= B3;                                   \
      B1 ^= B4;                                   \
      B0 = B2;                                    \
      B2 = B1;                                    \
      B1 = B3;                                    \
      B3 = ~B4;                                   \
   } while(0);

#define SBoxE4(B0, B1, B2, B3)                    \
   do {                                           \
      auto B4 = B0;                               \
      B0 |= B3;                                   \
      B3 ^= B1;                                   \
      B1 &= B4;                                   \
      B4 ^= B2;                                   \
      B2 ^= B3;                                   \
      B3 &= B0;                                   \
      B4 |= B1;                                   \
      B3 ^= B4;                                   \
      B0 ^= B1;                                   \
      B4 &= B0;                                   \
      B1 ^= B3;                                   \
      B4 ^= B2;                                   \
      B1 |= B0;                                   \
      B1 ^= B2;                                   \
      B0 ^= B3;                                   \
      B2 = B1;                                    \
      B1 |= B3;                                   \
      B0 ^= B1;                                   \
      B1 = B2;                                    \
      B2 = B3;                                    \
      B3 = B4;                                    \
   } while(0);

#define SBoxE5(B0, B1, B2, B3)                    \
   do {                                           \
      B1 ^= B3;                                   \
      B3 = ~B3;                                   \
      B2 ^= B3;                                   \
      B3 ^= B0;                                   \
      auto B4 = B1;                               \
      B1 &= B3;                                   \
      B1 ^= B2;                                   \
      B4 ^= B3;                                   \
      B0 ^= B4;                                   \
      B2 &= B4;                                   \
      B2 ^= B0;                                   \
      B0 &= B1;                                   \
      B3 ^= B0;                                   \
      B4 |= B1;                                   \
      B4 ^= B0;                                   \
      B0 |= B3;                                   \
      B0 ^= B2;                                   \
      B2 &= B3;                                   \
      B0 = ~B0;                                   \
      B4 ^= B2;                                   \
      B2 = B0;                                    \
      B0 = B1;                                    \
      B1 = B4;                                    \
   } while(0);

#define SBoxE6(B0, B1, B2, B3)                    \
   do {                                           \
      B0 ^= B1;                                   \
      B1 ^= B3;                                   \
      B3 = ~B3;                                   \
      auto B4 = B1;                               \
      B1 &= B0;                                   \
      B2 ^= B3;                                   \
      B1 ^= B2;                                   \
      B2 |= B4;                                   \
      B4 ^= B3;                                   \
      B3 &= B1;                                   \
      B3 ^= B0;                                   \
      B4 ^= B1;                                   \
      B4 ^= B2;                                   \
      B2 ^= B0;                                   \
      B0 &= B3;                                   \
      B2 = ~B2;                                   \
      B0 ^= B4;                                   \
      B4 |= B3;                                   \
      B4 ^= B2;                                   \
      B2 = B0;                                    \
      B0 = B1;                                    \
      B1 = B3;                                    \
      B3 = B4;                                    \
   } while(0);

#define SBoxE7(B0, B1, B2, B3)                    \
   do {                                           \
      B2 = ~B2;                                   \
      auto B4 = B3;                               \
      B3 &= B0;                                   \
      B0 ^= B4;                                   \
      B3 ^= B2;                                   \
      B2 |= B4;                                   \
      B1 ^= B3;                                   \
      B2 ^= B0;                                   \
      B0 |= B1;                                   \
      B2 ^= B1;                                   \
      B4 ^= B0;                                   \
      B0 |= B3;                                   \
      B0 ^= B2;                                   \
      B4 ^= B3;                                   \
      B4 ^= B0;                                   \
      B3 = ~B3;                                   \
      B2 &= B4;                                   \
      B3 ^= B2;                                   \
      B2 = B4;                                    \
   } while(0);

#define SBoxE8(B0, B1, B2, B3)                    \
   do {                                           \
      auto B4 = B1;                               \
      B1 |= B2;                                   \
      B1 ^= B3;                                   \
      B4 ^= B2;                                   \
      B2 ^= B1;                                   \
      B3 |= B4;                                   \
      B3 &= B0;                                   \
      B4 ^= B2;                                   \
      B3 ^= B1;                                   \
      B1 |= B4;                                   \
      B1 ^= B0;                                   \
      B0 |= B4;                                   \
      B0 ^= B2;                                   \
      B1 ^= B4;                                   \
      B2 ^= B1;                                   \
      B1 &= B0;                                   \
      B1 ^= B4;                                   \
      B2 = ~B2;                                   \
      B2 |= B0;                                   \
      B4 ^= B2;                                   \
      B2 = B1;                                    \
      B1 = B3;                                    \
      B3 = B0;                                    \
      B0 = B4;                                    \
   } while(0);

#define SBoxD1(B0, B1, B2, B3)                    \
   do {                                           \
      B2 = ~B2;                                   \
      auto B4 = B1;                               \
      B1 |= B0;                                   \
      B4 = ~B4;                                   \
      B1 ^= B2;                                   \
      B2 |= B4;                                   \
      B1 ^= B3;                                   \
      B0 ^= B4;                                   \
      B2 ^= B0;                                   \
      B0 &= B3;                                   \
      B4 ^= B0;                                   \
      B0 |= B1;                                   \
      B0 ^= B2;                                   \
      B3 ^= B4;                                   \
      B2 ^= B1;                                   \
      B3 ^= B0;                                   \
      B3 ^= B1;                                   \
      B2 &= B3;                                   \
      B4 ^= B2;                                   \
      B2 = B1;                                    \
      B1 = B4;                                    \
      } while(0);

#define SBoxD2(B0, B1, B2, B3)                    \
   do {                                           \
      auto B4 = B1;                               \
      B1 ^= B3;                                   \
      B3 &= B1;                                   \
      B4 ^= B2;                                   \
      B3 ^= B0;                                   \
      B0 |= B1;                                   \
      B2 ^= B3;                                   \
      B0 ^= B4;                                   \
      B0 |= B2;                                   \
      B1 ^= B3;                                   \
      B0 ^= B1;                                   \
      B1 |= B3;                                   \
      B1 ^= B0;                                   \
      B4 = ~B4;                                   \
      B4 ^= B1;                                   \
      B1 |= B0;                                   \
      B1 ^= B0;                                   \
      B1 |= B4;                                   \
      B3 ^= B1;                                   \
      B1 = B0;                                    \
      B0 = B4;                                    \
      B4 = B2;                                    \
      B2 = B3;                                    \
      B3 = B4;                                    \
      } while(0);

#define SBoxD3(B0, B1, B2, B3)                    \
   do {                                           \
      B2 ^= B3;                                   \
      B3 ^= B0;                                   \
      auto B4 = B3;                               \
      B3 &= B2;                                   \
      B3 ^= B1;                                   \
      B1 |= B2;                                   \
      B1 ^= B4;                                   \
      B4 &= B3;                                   \
      B2 ^= B3;                                   \
      B4 &= B0;                                   \
      B4 ^= B2;                                   \
      B2 &= B1;                                   \
      B2 |= B0;                                   \
      B3 = ~B3;                                   \
      B2 ^= B3;                                   \
      B0 ^= B3;                                   \
      B0 &= B1;                                   \
      B3 ^= B4;                                   \
      B3 ^= B0;                                   \
      B0 = B1;                                    \
      B1 = B4;                                    \
      } while(0);

#define SBoxD4(B0, B1, B2, B3)                    \
   do {                                           \
      auto B4 = B2;                               \
      B2 ^= B1;                                   \
      B0 ^= B2;                                   \
      B4 &= B2;                                   \
      B4 ^= B0;                                   \
      B0 &= B1;                                   \
      B1 ^= B3;                                   \
      B3 |= B4;                                   \
      B2 ^= B3;                                   \
      B0 ^= B3;                                   \
      B1 ^= B4;                                   \
      B3 &= B2;                                   \
      B3 ^= B1;                                   \
      B1 ^= B0;                                   \
      B1 |= B2;                                   \
      B0 ^= B3;                                   \
      B1 ^= B4;                                   \
      B0 ^= B1;                                   \
      B4 = B0;                                    \
      B0 = B2;                                    \
      B2 = B3;                                    \
      B3 = B4;                                    \
      } while(0);

#define SBoxD5(B0, B1, B2, B3)                    \
   do {                                           \
      auto B4 = B2;                               \
      B2 &= B3;                                   \
      B2 ^= B1;                                   \
      B1 |= B3;                                   \
      B1 &= B0;                                   \
      B4 ^= B2;                                   \
      B4 ^= B1;                                   \
      B1 &= B2;                                   \
      B0 = ~B0;                                   \
      B3 ^= B4;                                   \
      B1 ^= B3;                                   \
      B3 &= B0;                                   \
      B3 ^= B2;                                   \
      B0 ^= B1;                                   \
      B2 &= B0;                                   \
      B3 ^= B0;                                   \
      B2 ^= B4;                                   \
      B2 |= B3;                                   \
      B3 ^= B0;                                   \
      B2 ^= B1;                                   \
      B1 = B3;                                    \
      B3 = B4;                                    \
      } while(0);

#define SBoxD6(B0, B1, B2, B3)                    \
   do {                                           \
      B1 = ~B1;                                   \
      auto B4 = B3;                               \
      B2 ^= B1;                                   \
      B3 |= B0;                                   \
      B3 ^= B2;                                   \
      B2 |= B1;                                   \
      B2 &= B0;                                   \
      B4 ^= B3;                                   \
      B2 ^= B4;                                   \
      B4 |= B0;                                   \
      B4 ^= B1;                                   \
      B1 &= B2;                                   \
      B1 ^= B3;                                   \
      B4 ^= B2;                                   \
      B3 &= B4;                                   \
      B4 ^= B1;                                   \
      B3 ^= B4;                                   \
      B4 = ~B4;                                   \
      B3 ^= B0;                                   \
      B0 = B1;                                    \
      B1 = B4;                                    \
      B4 = B3;                                    \
      B3 = B2;                                    \
      B2 = B4;                                    \
      } while(0);

#define SBoxD7(B0, B1, B2, B3)                    \
   do {                                           \
      B0 ^= B2;                                   \
      auto B4 = B2;                               \
      B2 &= B0;                                   \
      B4 ^= B3;                                   \
      B2 = ~B2;                                   \
      B3 ^= B1;                                   \
      B2 ^= B3;                                   \
      B4 |= B0;                                   \
      B0 ^= B2;                                   \
      B3 ^= B4;                                   \
      B4 ^= B1;                                   \
      B1 &= B3;                                   \
      B1 ^= B0;                                   \
      B0 ^= B3;                                   \
      B0 |= B2;                                   \
      B3 ^= B1;                                   \
      B4 ^= B0;                                   \
      B0 = B1;                                    \
      B1 = B2;                                    \
      B2 = B4;                                    \
      } while(0);

#define SBoxD8(B0, B1, B2, B3)                    \
   do {                                           \
      auto B4 = B2;                               \
      B2 ^= B0;                                   \
      B0 &= B3;                                   \
      B4 |= B3;                                   \
      B2 = ~B2;                                   \
      B3 ^= B1;                                   \
      B1 |= B0;                                   \
      B0 ^= B2;                                   \
      B2 &= B4;                                   \
      B3 &= B4;                                   \
      B1 ^= B2;                                   \
      B2 ^= B0;                                   \
      B0 |= B2;                                   \
      B4 ^= B1;                                   \
      B0 ^= B3;                                   \
      B3 ^= B4;                                   \
      B4 |= B0;                                   \
      B3 ^= B2;                                   \
      B4 ^= B2;                                   \
      B2 = B1;                                    \
      B1 = B0;                                    \
      B0 = B3;                                    \
      B3 = B4;                                    \
      } while(0);


#if defined(BOTAN_HAS_SIMD_SSE2)
#if defined(BOTAN_TARGET_SUPPORTS_SSE2)

#include <emmintrin.h>

namespace Botan {

class SIMD_SSE2
   {
   public:
      explicit SIMD_SSE2(const u32bit B[4])
         {
         m_reg = _mm_loadu_si128(reinterpret_cast<const __m128i*>(B));
         }

      SIMD_SSE2(u32bit B0, u32bit B1, u32bit B2, u32bit B3)
         {
         m_reg = _mm_set_epi32(B0, B1, B2, B3);
         }

      explicit SIMD_SSE2(u32bit B)
         {
         m_reg = _mm_set1_epi32(B);
         }

      static SIMD_SSE2 load_le(const void* in)
         {
         return SIMD_SSE2(_mm_loadu_si128(reinterpret_cast<const __m128i*>(in)));
         }

      static SIMD_SSE2 load_be(const void* in)
         {
         return load_le(in).bswap();
         }

      void store_le(byte out[]) const
         {
         _mm_storeu_si128(reinterpret_cast<__m128i*>(out), m_reg);
         }

      void store_be(byte out[]) const
         {
         bswap().store_le(out);
         }

      void rotate_left(size_t rot)
         {
         m_reg = _mm_or_si128(_mm_slli_epi32(m_reg, static_cast<int>(rot)),
                            _mm_srli_epi32(m_reg, static_cast<int>(32-rot)));
         }

      void rotate_right(size_t rot)
         {
         rotate_left(32 - rot);
         }

      void operator+=(const SIMD_SSE2& other)
         {
         m_reg = _mm_add_epi32(m_reg, other.m_reg);
         }

      SIMD_SSE2 operator+(const SIMD_SSE2& other) const
         {
         return SIMD_SSE2(_mm_add_epi32(m_reg, other.m_reg));
         }

      void operator-=(const SIMD_SSE2& other)
         {
         m_reg = _mm_sub_epi32(m_reg, other.m_reg);
         }

      SIMD_SSE2 operator-(const SIMD_SSE2& other) const
         {
         return SIMD_SSE2(_mm_sub_epi32(m_reg, other.m_reg));
         }

      void operator^=(const SIMD_SSE2& other)
         {
         m_reg = _mm_xor_si128(m_reg, other.m_reg);
         }

      SIMD_SSE2 operator^(const SIMD_SSE2& other) const
         {
         return SIMD_SSE2(_mm_xor_si128(m_reg, other.m_reg));
         }

      void operator|=(const SIMD_SSE2& other)
         {
         m_reg = _mm_or_si128(m_reg, other.m_reg);
         }

      SIMD_SSE2 operator&(const SIMD_SSE2& other)
         {
         return SIMD_SSE2(_mm_and_si128(m_reg, other.m_reg));
         }

      void operator&=(const SIMD_SSE2& other)
         {
         m_reg = _mm_and_si128(m_reg, other.m_reg);
         }

      SIMD_SSE2 operator<<(size_t shift) const
         {
         return SIMD_SSE2(_mm_slli_epi32(m_reg, static_cast<int>(shift)));
         }

      SIMD_SSE2 operator>>(size_t shift) const
         {
         return SIMD_SSE2(_mm_srli_epi32(m_reg, static_cast<int>(shift)));
         }

      SIMD_SSE2 operator~() const
         {
         return SIMD_SSE2(_mm_xor_si128(m_reg, _mm_set1_epi32(0xFFFFFFFF)));
         }

      // (~reg) & other
      SIMD_SSE2 andc(const SIMD_SSE2& other)
         {
         return SIMD_SSE2(_mm_andnot_si128(m_reg, other.m_reg));
         }

      SIMD_SSE2 bswap() const
         {
         __m128i T = m_reg;

         T = _mm_shufflehi_epi16(T, _MM_SHUFFLE(2, 3, 0, 1));
         T = _mm_shufflelo_epi16(T, _MM_SHUFFLE(2, 3, 0, 1));

         return SIMD_SSE2(_mm_or_si128(_mm_srli_epi16(T, 8),
                             _mm_slli_epi16(T, 8)));
         }

      static void transpose(SIMD_SSE2& B0, SIMD_SSE2& B1,
                            SIMD_SSE2& B2, SIMD_SSE2& B3)
         {
         __m128i T0 = _mm_unpacklo_epi32(B0.m_reg, B1.m_reg);
         __m128i T1 = _mm_unpacklo_epi32(B2.m_reg, B3.m_reg);
         __m128i T2 = _mm_unpackhi_epi32(B0.m_reg, B1.m_reg);
         __m128i T3 = _mm_unpackhi_epi32(B2.m_reg, B3.m_reg);
         B0.m_reg = _mm_unpacklo_epi64(T0, T1);
         B1.m_reg = _mm_unpackhi_epi64(T0, T1);
         B2.m_reg = _mm_unpacklo_epi64(T2, T3);
         B3.m_reg = _mm_unpackhi_epi64(T2, T3);
         }

   private:
      explicit SIMD_SSE2(__m128i in) { m_reg = in; }

      __m128i m_reg;
   };

}

#endif

  namespace Botan { typedef SIMD_SSE2 SIMD_32; }

#elif defined(BOTAN_HAS_SIMD_ALTIVEC)
  namespace Botan { typedef SIMD_Altivec SIMD_32; }

#elif defined(BOTAN_HAS_SIMD_SCALAR)
  namespace Botan { typedef SIMD_Scalar<u32bit,4> SIMD_32; }

#else
  #error "No SIMD module defined"

#endif


namespace Botan {

inline std::vector<byte> to_byte_vector(const std::string& s)
   {
   return std::vector<byte>(s.cbegin(), s.cend());
   }

inline std::string to_string(const secure_vector<byte> &bytes)
   {
   return std::string(bytes.cbegin(), bytes.cend());
   }

/**
* Return the keys of a map as a std::set
*/
template<typename K, typename V>
std::set<K> map_keys_as_set(const std::map<K, V>& kv)
   {
   std::set<K> s;
   for(auto&& i : kv)
      {
      s.insert(i.first);
      }
   return s;
   }

/*
* Searching through a std::map
* @param mapping the map to search
* @param key is what to look for
* @param null_result is the value to return if key is not in mapping
* @return mapping[key] or null_result
*/
template<typename K, typename V>
inline V search_map(const std::map<K, V>& mapping,
                    const K& key,
                    const V& null_result = V())
   {
   auto i = mapping.find(key);
   if(i == mapping.end())
      return null_result;
   return i->second;
   }

template<typename K, typename V, typename R>
inline R search_map(const std::map<K, V>& mapping, const K& key,
                    const R& null_result, const R& found_result)
   {
   auto i = mapping.find(key);
   if(i == mapping.end())
      return null_result;
   return found_result;
   }

/*
* Insert a key/value pair into a multimap
*/
template<typename K, typename V>
void multimap_insert(std::multimap<K, V>& multimap,
                     const K& key, const V& value)
   {
#if defined(BOTAN_BUILD_COMPILER_IS_SUN_STUDIO)
   // Work around a strange bug in Sun Studio
   multimap.insert(std::make_pair<const K, V>(key, value));
#else
   multimap.insert(std::make_pair(key, value));
#endif
   }

/**
* Existence check for values
*/
template<typename T>
bool value_exists(const std::vector<T>& vec,
                  const T& val)
   {
   for(size_t i = 0; i != vec.size(); ++i)
      if(vec[i] == val)
         return true;
   return false;
   }

template<typename T, typename Pred>
void map_remove_if(Pred pred, T& assoc)
   {
   auto i = assoc.begin();
   while(i != assoc.end())
      {
      if(pred(i->first))
         assoc.erase(i++);
      else
         i++;
      }
   }

}


namespace Botan {

namespace TLS {

class TLS_Data_Reader;

enum Handshake_Extension_Type {
   TLSEXT_SERVER_NAME_INDICATION = 0,
   // 1 is maximum fragment length
   TLSEXT_CLIENT_CERT_URL        = 2,
   TLSEXT_TRUSTED_CA_KEYS        = 3,
   TLSEXT_TRUNCATED_HMAC         = 4,

   TLSEXT_CERTIFICATE_TYPES      = 9,
   TLSEXT_USABLE_ELLIPTIC_CURVES = 10,
   TLSEXT_EC_POINT_FORMATS       = 11,
   TLSEXT_SRP_IDENTIFIER         = 12,
   TLSEXT_SIGNATURE_ALGORITHMS   = 13,
   TLSEXT_USE_SRTP               = 14,
   TLSEXT_HEARTBEAT_SUPPORT      = 15,
   TLSEXT_ALPN                   = 16,

   TLSEXT_EXTENDED_MASTER_SECRET = 23,

   TLSEXT_SESSION_TICKET         = 35,

   TLSEXT_SAFE_RENEGOTIATION     = 65281,
};

/**
* Base class representing a TLS extension of some kind
*/
class Extension
   {
   public:
      /**
      * @return code number of the extension
      */
      virtual Handshake_Extension_Type type() const = 0;

      /**
      * @return serialized binary for the extension
      */
      virtual std::vector<byte> serialize() const = 0;

      /**
      * @return if we should encode this extension or not
      */
      virtual bool empty() const = 0;

      virtual ~Extension() {}
   };

/**
* Server Name Indicator extension (RFC 3546)
*/
class Server_Name_Indicator final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type()
         { return TLSEXT_SERVER_NAME_INDICATION; }

      Handshake_Extension_Type type() const override { return static_type(); }

      explicit Server_Name_Indicator(const std::string& host_name) :
         m_sni_host_name(host_name) {}

      Server_Name_Indicator(TLS_Data_Reader& reader,
                            u16bit extension_size);

      std::string host_name() const { return m_sni_host_name; }

      std::vector<byte> serialize() const override;

      bool empty() const override { return m_sni_host_name.empty(); }
   private:
      std::string m_sni_host_name;
   };

#if defined(BOTAN_HAS_SRP6)
/**
* SRP identifier extension (RFC 5054)
*/
class SRP_Identifier final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type()
         { return TLSEXT_SRP_IDENTIFIER; }

      Handshake_Extension_Type type() const override { return static_type(); }

      explicit SRP_Identifier(const std::string& identifier) :
         m_srp_identifier(identifier) {}

      SRP_Identifier(TLS_Data_Reader& reader,
                     u16bit extension_size);

      std::string identifier() const { return m_srp_identifier; }

      std::vector<byte> serialize() const override;

      bool empty() const override { return m_srp_identifier.empty(); }
   private:
      std::string m_srp_identifier;
   };
#endif

/**
* Renegotiation Indication Extension (RFC 5746)
*/
class Renegotiation_Extension final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type()
         { return TLSEXT_SAFE_RENEGOTIATION; }

      Handshake_Extension_Type type() const override { return static_type(); }

      Renegotiation_Extension() {}

      explicit Renegotiation_Extension(const std::vector<byte>& bits) :
         m_reneg_data(bits) {}

      Renegotiation_Extension(TLS_Data_Reader& reader,
                             u16bit extension_size);

      const std::vector<byte>& renegotiation_info() const
         { return m_reneg_data; }

      std::vector<byte> serialize() const override;

      bool empty() const override { return false; } // always send this
   private:
      std::vector<byte> m_reneg_data;
   };

/**
* ALPN (RFC 7301)
*/
class Application_Layer_Protocol_Notification final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type() { return TLSEXT_ALPN; }

      Handshake_Extension_Type type() const override { return static_type(); }

      const std::vector<std::string>& protocols() const { return m_protocols; }

      const std::string& single_protocol() const;

      /**
      * Single protocol, used by server
      */
      explicit Application_Layer_Protocol_Notification(const std::string& protocol) :
         m_protocols(1, protocol) {}

      /**
      * List of protocols, used by client
      */
      explicit Application_Layer_Protocol_Notification(const std::vector<std::string>& protocols) :
         m_protocols(protocols) {}

      Application_Layer_Protocol_Notification(TLS_Data_Reader& reader,
                                              u16bit extension_size);

      std::vector<byte> serialize() const override;

      bool empty() const override { return m_protocols.empty(); }
   private:
      std::vector<std::string> m_protocols;
   };

/**
* Session Ticket Extension (RFC 5077)
*/
class Session_Ticket final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type()
         { return TLSEXT_SESSION_TICKET; }

      Handshake_Extension_Type type() const override { return static_type(); }

      /**
      * @return contents of the session ticket
      */
      const std::vector<byte>& contents() const { return m_ticket; }

      /**
      * Create empty extension, used by both client and server
      */
      Session_Ticket() {}

      /**
      * Extension with ticket, used by client
      */
      explicit Session_Ticket(const std::vector<byte>& session_ticket) :
         m_ticket(session_ticket) {}

      /**
      * Deserialize a session ticket
      */
      Session_Ticket(TLS_Data_Reader& reader, u16bit extension_size);

      std::vector<byte> serialize() const override { return m_ticket; }

      bool empty() const override { return false; }
   private:
      std::vector<byte> m_ticket;
   };

/**
* Supported Elliptic Curves Extension (RFC 4492)
*/
class Supported_Elliptic_Curves final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type()
         { return TLSEXT_USABLE_ELLIPTIC_CURVES; }

      Handshake_Extension_Type type() const override { return static_type(); }

      static std::string curve_id_to_name(u16bit id);
      static u16bit name_to_curve_id(const std::string& name);

      const std::vector<std::string>& curves() const { return m_curves; }

      std::vector<byte> serialize() const override;

      explicit Supported_Elliptic_Curves(const std::vector<std::string>& curves) :
         m_curves(curves) {}

      Supported_Elliptic_Curves(TLS_Data_Reader& reader,
                                u16bit extension_size);

      bool empty() const override { return m_curves.empty(); }
   private:
      std::vector<std::string> m_curves;
   };

/**
* Signature Algorithms Extension for TLS 1.2 (RFC 5246)
*/
class Signature_Algorithms final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type()
         { return TLSEXT_SIGNATURE_ALGORITHMS; }

      Handshake_Extension_Type type() const override { return static_type(); }

      static std::string hash_algo_name(byte code);
      static byte hash_algo_code(const std::string& name);

      static std::string sig_algo_name(byte code);
      static byte sig_algo_code(const std::string& name);

      std::vector<std::pair<std::string, std::string> >
         supported_signature_algorthms() const
         {
         return m_supported_algos;
         }

      std::vector<byte> serialize() const override;

      bool empty() const override { return false; }

      Signature_Algorithms(const std::vector<std::string>& hashes,
                           const std::vector<std::string>& sig_algos);

      explicit Signature_Algorithms(const std::vector<std::pair<std::string, std::string> >& algos) :
         m_supported_algos(algos) {}

      Signature_Algorithms(TLS_Data_Reader& reader,
                           u16bit extension_size);
   private:
      std::vector<std::pair<std::string, std::string> > m_supported_algos;
   };

/**
* Used to indicate SRTP algorithms for DTLS (RFC 5764)
*/
class SRTP_Protection_Profiles final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type()
         { return TLSEXT_USE_SRTP; }

      Handshake_Extension_Type type() const override { return static_type(); }

      const std::vector<u16bit>& profiles() const { return m_pp; }

      std::vector<byte> serialize() const override;

      bool empty() const override { return m_pp.empty(); }

      explicit SRTP_Protection_Profiles(const std::vector<u16bit>& pp) : m_pp(pp) {}

      explicit SRTP_Protection_Profiles(u16bit pp) : m_pp(1, pp) {}

      SRTP_Protection_Profiles(TLS_Data_Reader& reader, u16bit extension_size);
   private:
      std::vector<u16bit> m_pp;
   };

/**
* Extended Master Secret Extension (RFC 7627)
*/
class Extended_Master_Secret final : public Extension
   {
   public:
      static Handshake_Extension_Type static_type()
         { return TLSEXT_EXTENDED_MASTER_SECRET; }

      Handshake_Extension_Type type() const override { return static_type(); }

      std::vector<byte> serialize() const override;

      bool empty() const override { return false; }

      Extended_Master_Secret() {}

      Extended_Master_Secret(TLS_Data_Reader& reader, u16bit extension_size);
   };

/**
* Represents a block of extensions in a hello message
*/
class Extensions
   {
   public:
      std::set<Handshake_Extension_Type> extension_types() const;

      template<typename T>
      T* get() const
         {
         Handshake_Extension_Type type = T::static_type();

         auto i = m_extensions.find(type);

         if(i != m_extensions.end())
            return dynamic_cast<T*>(i->second.get());
         return nullptr;
         }

      template<typename T>
      bool has() const
         {
         return get<T>() != nullptr;
         }

      void add(Extension* extn)
         {
         m_extensions[extn->type()].reset(extn);
         }

      std::vector<byte> serialize() const;

      void deserialize(TLS_Data_Reader& reader);

      Extensions() {}

      explicit Extensions(TLS_Data_Reader& reader) { deserialize(reader); }

   private:
      Extensions(const Extensions&) {}
      Extensions& operator=(const Extensions&) { return (*this); }

      std::map<Handshake_Extension_Type, std::unique_ptr<Extension>> m_extensions;
   };

}

}


namespace Botan {

namespace TLS {

/**
* TLS Handshake Hash
*/
class Handshake_Hash
   {
   public:
      void update(const byte in[], size_t length)
         { m_data += std::make_pair(in, length); }

      void update(const std::vector<byte>& in)
         { m_data += in; }

      secure_vector<byte> final(Protocol_Version version,
                                const std::string& mac_algo) const;

      const std::vector<byte>& get_contents() const { return m_data; }

      void reset() { m_data.clear(); }
   private:
      std::vector<byte> m_data;
   };

}

}


namespace Botan {

namespace TLS {

class Handshake_Message;

/**
* Handshake IO Interface
*/
class Handshake_IO
   {
   public:
      virtual Protocol_Version initial_record_version() const = 0;

      virtual std::vector<byte> send(const Handshake_Message& msg) = 0;

      virtual bool timeout_check() = 0;

      virtual std::vector<byte> format(
         const std::vector<byte>& handshake_msg,
         Handshake_Type handshake_type) const = 0;

      virtual void add_record(const std::vector<byte>& record,
                              Record_Type type,
                              u64bit sequence_number) = 0;

      /**
      * Returns (HANDSHAKE_NONE, std::vector<>()) if no message currently available
      */
      virtual std::pair<Handshake_Type, std::vector<byte>>
         get_next_record(bool expecting_ccs) = 0;

      Handshake_IO() {}

      Handshake_IO(const Handshake_IO&) = delete;

      Handshake_IO& operator=(const Handshake_IO&) = delete;

      virtual ~Handshake_IO() {}
   };

/**
* Handshake IO for stream-based handshakes
*/
class Stream_Handshake_IO final : public Handshake_IO
   {
   public:
      typedef std::function<void (byte, const std::vector<byte>&)> writer_fn;

      explicit Stream_Handshake_IO(writer_fn writer) : m_send_hs(writer) {}

      Protocol_Version initial_record_version() const override;

      bool timeout_check() override { return false; }

      std::vector<byte> send(const Handshake_Message& msg) override;

      std::vector<byte> format(
         const std::vector<byte>& handshake_msg,
         Handshake_Type handshake_type) const override;

      void add_record(const std::vector<byte>& record,
                      Record_Type type,
                      u64bit sequence_number) override;

      std::pair<Handshake_Type, std::vector<byte>>
         get_next_record(bool expecting_ccs) override;
   private:
      std::deque<byte> m_queue;
      writer_fn m_send_hs;
   };

/**
* Handshake IO for datagram-based handshakes
*/
class Datagram_Handshake_IO final : public Handshake_IO
   {
   public:
      typedef std::function<void (u16bit, byte, const std::vector<byte>&)> writer_fn;

      Datagram_Handshake_IO(writer_fn writer,
                            class Connection_Sequence_Numbers& seq,
                            u16bit mtu, u64bit initial_timeout_ms, u64bit max_timeout_ms) :
         m_seqs(seq),
         m_flights(1),
         m_initial_timeout(initial_timeout_ms),
         m_max_timeout(max_timeout_ms),
         m_send_hs(writer),
         m_mtu(mtu)
         {}

      Protocol_Version initial_record_version() const override;

      bool timeout_check() override;

      std::vector<byte> send(const Handshake_Message& msg) override;

      std::vector<byte> format(
         const std::vector<byte>& handshake_msg,
         Handshake_Type handshake_type) const override;

      void add_record(const std::vector<byte>& record,
                      Record_Type type,
                      u64bit sequence_number) override;

      std::pair<Handshake_Type, std::vector<byte>>
         get_next_record(bool expecting_ccs) override;
   private:
      void retransmit_flight(size_t flight);
      void retransmit_last_flight();

      std::vector<byte> format_fragment(
         const byte fragment[],
         size_t fragment_len,
         u16bit frag_offset,
         u16bit msg_len,
         Handshake_Type type,
         u16bit msg_sequence) const;

      std::vector<byte> format_w_seq(
         const std::vector<byte>& handshake_msg,
         Handshake_Type handshake_type,
         u16bit msg_sequence) const;

      std::vector<byte> send_message(u16bit msg_seq, u16bit epoch,
                                     Handshake_Type msg_type,
                                     const std::vector<byte>& msg);

      class Handshake_Reassembly
         {
         public:
            void add_fragment(const byte fragment[],
                              size_t fragment_length,
                              size_t fragment_offset,
                              u16bit epoch,
                              byte msg_type,
                              size_t msg_length);

            bool complete() const;

            u16bit epoch() const { return m_epoch; }

            std::pair<Handshake_Type, std::vector<byte>> message() const;
         private:
            byte m_msg_type = HANDSHAKE_NONE;
            size_t m_msg_length = 0;
            u16bit m_epoch = 0;

            // vector<bool> m_seen;
            // vector<byte> m_fragments
            std::map<size_t, byte> m_fragments;
            std::vector<byte> m_message;
         };

      struct Message_Info
         {
         Message_Info(u16bit e, Handshake_Type mt, const std::vector<byte>& msg) :
            epoch(e), msg_type(mt), msg_bits(msg) {}

         Message_Info(const Message_Info& other) = default;

         Message_Info() : epoch(0xFFFF), msg_type(HANDSHAKE_NONE) {}

         u16bit epoch;
         Handshake_Type msg_type;
         std::vector<byte> msg_bits;
         };

      class Connection_Sequence_Numbers& m_seqs;
      std::map<u16bit, Handshake_Reassembly> m_messages;
      std::set<u16bit> m_ccs_epochs;
      std::vector<std::vector<u16bit>> m_flights;
      std::map<u16bit, Message_Info> m_flight_data;

      u64bit m_initial_timeout = 0;
      u64bit m_max_timeout = 0;

      u64bit m_last_write = 0;
      u64bit m_next_timeout = 0;

      u16bit m_in_message_seq = 0;
      u16bit m_out_message_seq = 0;

      writer_fn m_send_hs;
      u16bit m_mtu;
   };

}

}


namespace Botan {

namespace TLS {

/**
* TLS Session Keys
*/
class Session_Keys
   {
   public:
      SymmetricKey client_cipher_key() const { return m_c_cipher; }
      SymmetricKey server_cipher_key() const { return m_s_cipher; }

      SymmetricKey client_mac_key() const { return m_c_mac; }
      SymmetricKey server_mac_key() const { return m_s_mac; }

      InitializationVector client_iv() const { return m_c_iv; }
      InitializationVector server_iv() const { return m_s_iv; }

      const secure_vector<byte>& master_secret() const { return m_master_sec; }

      Session_Keys() {}

      Session_Keys(const class Handshake_State* state,
                   const secure_vector<byte>& pre_master,
                   bool resuming);

   private:
      secure_vector<byte> m_master_sec;
      SymmetricKey m_c_cipher, m_s_cipher, m_c_mac, m_s_mac;
      InitializationVector m_c_iv, m_s_iv;
   };

}

}


namespace Botan {

class KDF;

namespace TLS {

class Policy;

class Hello_Verify_Request;
class Client_Hello;
class Server_Hello;
class Certificate;
class Server_Key_Exchange;
class Certificate_Req;
class Server_Hello_Done;
class Certificate;
class Client_Key_Exchange;
class Certificate_Verify;
class New_Session_Ticket;
class Finished;

/**
* SSL/TLS Handshake State
*/
class Handshake_State
   {
   public:
      typedef std::function<void (const Handshake_Message&)> handshake_msg_cb;

      Handshake_State(Handshake_IO* io, handshake_msg_cb cb);

      virtual ~Handshake_State();

      Handshake_State(const Handshake_State&) = delete;
      Handshake_State& operator=(const Handshake_State&) = delete;

      Handshake_IO& handshake_io() { return *m_handshake_io; }

      /**
      * Return true iff we have received a particular message already
      * @param msg_type the message type
      */
      bool received_handshake_msg(Handshake_Type msg_type) const;

      /**
      * Confirm that we were expecting this message type
      * @param msg_type the message type
      */
      void confirm_transition_to(Handshake_Type msg_type);

      /**
      * Record that we are expecting a particular message type next
      * @param msg_type the message type
      */
      void set_expected_next(Handshake_Type msg_type);

      std::pair<Handshake_Type, std::vector<byte>>
         get_next_handshake_msg();

      std::vector<byte> session_ticket() const;

      std::pair<std::string, Signature_Format>
         parse_sig_format(const Public_Key& key,
                          const std::string& hash_algo,
                          const std::string& sig_algo,
                          bool for_client_auth,
                          const Policy& policy) const;

      std::pair<std::string, Signature_Format>
         choose_sig_format(const Private_Key& key,
                           std::string& hash_algo,
                           std::string& sig_algo,
                           bool for_client_auth,
                           const Policy& policy) const;

      std::string srp_identifier() const;

      KDF* protocol_specific_prf() const;

      Protocol_Version version() const { return m_version; }

      void set_version(const Protocol_Version& version);

      void hello_verify_request(const Hello_Verify_Request& hello_verify);

      void client_hello(Client_Hello* client_hello);
      void server_hello(Server_Hello* server_hello);
      void server_certs(Certificate* server_certs);
      void server_kex(Server_Key_Exchange* server_kex);
      void cert_req(Certificate_Req* cert_req);
      void server_hello_done(Server_Hello_Done* server_hello_done);
      void client_certs(Certificate* client_certs);
      void client_kex(Client_Key_Exchange* client_kex);
      void client_verify(Certificate_Verify* client_verify);
      void new_session_ticket(New_Session_Ticket* new_session_ticket);
      void server_finished(Finished* server_finished);
      void client_finished(Finished* client_finished);

      const Client_Hello* client_hello() const
         { return m_client_hello.get(); }

      const Server_Hello* server_hello() const
         { return m_server_hello.get(); }

      const Certificate* server_certs() const
         { return m_server_certs.get(); }

      const Server_Key_Exchange* server_kex() const
         { return m_server_kex.get(); }

      const Certificate_Req* cert_req() const
         { return m_cert_req.get(); }

      const Server_Hello_Done* server_hello_done() const
         { return m_server_hello_done.get(); }

      const Certificate* client_certs() const
         { return m_client_certs.get(); }

      const Client_Key_Exchange* client_kex() const
         { return m_client_kex.get(); }

      const Certificate_Verify* client_verify() const
         { return m_client_verify.get(); }

      const New_Session_Ticket* new_session_ticket() const
         { return m_new_session_ticket.get(); }

      const Finished* server_finished() const
         { return m_server_finished.get(); }

      const Finished* client_finished() const
         { return m_client_finished.get(); }

      const Ciphersuite& ciphersuite() const { return m_ciphersuite; }

      const Session_Keys& session_keys() const { return m_session_keys; }

      void compute_session_keys();

      void compute_session_keys(const secure_vector<byte>& resume_master_secret);

      Handshake_Hash& hash() { return m_handshake_hash; }

      const Handshake_Hash& hash() const { return m_handshake_hash; }

      void note_message(const Handshake_Message& msg)
         {
         if(m_msg_callback)
            m_msg_callback(msg);
         }

   private:

      handshake_msg_cb m_msg_callback;

      std::unique_ptr<Handshake_IO> m_handshake_io;

      u32bit m_hand_expecting_mask = 0;
      u32bit m_hand_received_mask = 0;
      Protocol_Version m_version;
      Ciphersuite m_ciphersuite;
      Session_Keys m_session_keys;
      Handshake_Hash m_handshake_hash;

      std::unique_ptr<Client_Hello> m_client_hello;
      std::unique_ptr<Server_Hello> m_server_hello;
      std::unique_ptr<Certificate> m_server_certs;
      std::unique_ptr<Server_Key_Exchange> m_server_kex;
      std::unique_ptr<Certificate_Req> m_cert_req;
      std::unique_ptr<Server_Hello_Done> m_server_hello_done;
      std::unique_ptr<Certificate> m_client_certs;
      std::unique_ptr<Client_Key_Exchange> m_client_kex;
      std::unique_ptr<Certificate_Verify> m_client_verify;
      std::unique_ptr<New_Session_Ticket> m_new_session_ticket;
      std::unique_ptr<Finished> m_server_finished;
      std::unique_ptr<Finished> m_client_finished;
   };

}

}


namespace Botan {

class Credentials_Manager;

#if defined(BOTAN_HAS_SRP6)
class SRP6_Server_Session;
#endif

namespace TLS {

class Session;
class Handshake_IO;

std::vector<byte> make_hello_random(RandomNumberGenerator& rng,
                                    const Policy& policy);

/**
* DTLS Hello Verify Request
*/
class Hello_Verify_Request final : public Handshake_Message
   {
   public:
      std::vector<byte> serialize() const override;
      Handshake_Type type() const override { return HELLO_VERIFY_REQUEST; }

      std::vector<byte> cookie() const { return m_cookie; }

      explicit Hello_Verify_Request(const std::vector<byte>& buf);

      Hello_Verify_Request(const std::vector<byte>& client_hello_bits,
                           const std::string& client_identity,
                           const SymmetricKey& secret_key);
   private:
      std::vector<byte> m_cookie;
   };

/**
* Client Hello Message
*/
class Client_Hello final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return CLIENT_HELLO; }

      Protocol_Version version() const { return m_version; }

      const std::vector<byte>& random() const { return m_random; }

      const std::vector<byte>& session_id() const { return m_session_id; }

      std::vector<u16bit> ciphersuites() const { return m_suites; }

      std::vector<byte> compression_methods() const { return m_comp_methods; }

      bool offered_suite(u16bit ciphersuite) const;

      bool sent_fallback_scsv() const;

      std::vector<std::pair<std::string, std::string>> supported_algos() const
         {
         if(Signature_Algorithms* sigs = m_extensions.get<Signature_Algorithms>())
            return sigs->supported_signature_algorthms();
         return std::vector<std::pair<std::string, std::string>>();
         }

      std::vector<std::string> supported_ecc_curves() const
         {
         if(Supported_Elliptic_Curves* ecc = m_extensions.get<Supported_Elliptic_Curves>())
            return ecc->curves();
         return std::vector<std::string>();
         }

      std::string sni_hostname() const
         {
         if(Server_Name_Indicator* sni = m_extensions.get<Server_Name_Indicator>())
            return sni->host_name();
         return "";
         }

#if defined(BOTAN_HAS_SRP6)
      std::string srp_identifier() const
         {
         if(SRP_Identifier* srp = m_extensions.get<SRP_Identifier>())
            return srp->identifier();
         return "";
         }
#endif

      bool secure_renegotiation() const
         {
         return m_extensions.has<Renegotiation_Extension>();
         }

      std::vector<byte> renegotiation_info() const
         {
         if(Renegotiation_Extension* reneg = m_extensions.get<Renegotiation_Extension>())
            return reneg->renegotiation_info();
         return std::vector<byte>();
         }

      bool supports_session_ticket() const
         {
         return m_extensions.has<Session_Ticket>();
         }

      std::vector<byte> session_ticket() const
         {
         if(Session_Ticket* ticket = m_extensions.get<Session_Ticket>())
            return ticket->contents();
         return std::vector<byte>();
         }

      bool supports_alpn() const
         {
         return m_extensions.has<Application_Layer_Protocol_Notification>();
         }

      bool supports_extended_master_secret() const
         {
         return m_extensions.has<Extended_Master_Secret>();
         }

      std::vector<std::string> next_protocols() const
         {
         if(auto alpn = m_extensions.get<Application_Layer_Protocol_Notification>())
            return alpn->protocols();
         return std::vector<std::string>();
         }

      std::vector<u16bit> srtp_profiles() const
         {
         if(SRTP_Protection_Profiles* srtp = m_extensions.get<SRTP_Protection_Profiles>())
            return srtp->profiles();
         return std::vector<u16bit>();
         }

      void update_hello_cookie(const Hello_Verify_Request& hello_verify);

      std::set<Handshake_Extension_Type> extension_types() const
         { return m_extensions.extension_types(); }

      Client_Hello(Handshake_IO& io,
                   Handshake_Hash& hash,
                   Protocol_Version version,
                   const Policy& policy,
                   RandomNumberGenerator& rng,
                   const std::vector<byte>& reneg_info,
                   const std::vector<std::string>& next_protocols,
                   const std::string& hostname = "",
                   const std::string& srp_identifier = "");

      Client_Hello(Handshake_IO& io,
                   Handshake_Hash& hash,
                   const Policy& policy,
                   RandomNumberGenerator& rng,
                   const std::vector<byte>& reneg_info,
                   const Session& resumed_session,
                   const std::vector<std::string>& next_protocols);

      explicit Client_Hello(const std::vector<byte>& buf);

   private:
      std::vector<byte> serialize() const override;

      Protocol_Version m_version;
      std::vector<byte> m_session_id;
      std::vector<byte> m_random;
      std::vector<u16bit> m_suites;
      std::vector<byte> m_comp_methods;
      std::vector<byte> m_hello_cookie; // DTLS only

      Extensions m_extensions;
   };

/**
* Server Hello Message
*/
class Server_Hello final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return SERVER_HELLO; }

      Protocol_Version version() const { return m_version; }

      const std::vector<byte>& random() const { return m_random; }

      const std::vector<byte>& session_id() const { return m_session_id; }

      u16bit ciphersuite() const { return m_ciphersuite; }

      byte compression_method() const { return m_comp_method; }

      bool secure_renegotiation() const
         {
         return m_extensions.has<Renegotiation_Extension>();
         }

      std::vector<byte> renegotiation_info() const
         {
         if(Renegotiation_Extension* reneg = m_extensions.get<Renegotiation_Extension>())
            return reneg->renegotiation_info();
         return std::vector<byte>();
         }

      bool supports_extended_master_secret() const
         {
         return m_extensions.has<Extended_Master_Secret>();
         }

      bool supports_session_ticket() const
         {
         return m_extensions.has<Session_Ticket>();
         }

      u16bit srtp_profile() const
         {
         if(auto srtp = m_extensions.get<SRTP_Protection_Profiles>())
            {
            auto prof = srtp->profiles();
            if(prof.size() != 1 || prof[0] == 0)
               throw Decoding_Error("Server sent malformed DTLS-SRTP extension");
            return prof[0];
            }

         return 0;
         }

      std::string next_protocol() const
         {
         if(auto alpn = m_extensions.get<Application_Layer_Protocol_Notification>())
            return alpn->single_protocol();
         return "";
         }

      std::set<Handshake_Extension_Type> extension_types() const
         { return m_extensions.extension_types(); }

      Server_Hello(Handshake_IO& io,
                   Handshake_Hash& hash,
                   const Policy& policy,
                   RandomNumberGenerator& rng,
                   const std::vector<byte>& secure_reneg_info,
                   const Client_Hello& client_hello,
                   const std::vector<byte>& new_session_id,
                   Protocol_Version new_session_version,
                   u16bit ciphersuite,
                   byte compression,
                   bool offer_session_ticket,
                   const std::string& next_protocol);

      Server_Hello(Handshake_IO& io,
                   Handshake_Hash& hash,
                   const Policy& policy,
                   RandomNumberGenerator& rng,
                   const std::vector<byte>& secure_reneg_info,
                   const Client_Hello& client_hello,
                   Session& resumed_session,
                   bool offer_session_ticket,
                   const std::string& next_protocol);

      explicit Server_Hello(const std::vector<byte>& buf);
   private:
      std::vector<byte> serialize() const override;

      Protocol_Version m_version;
      std::vector<byte> m_session_id, m_random;
      u16bit m_ciphersuite;
      byte m_comp_method;

      Extensions m_extensions;
   };

/**
* Client Key Exchange Message
*/
class Client_Key_Exchange final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return CLIENT_KEX; }

      const secure_vector<byte>& pre_master_secret() const
         { return m_pre_master; }

      Client_Key_Exchange(Handshake_IO& io,
                          Handshake_State& state,
                          const Policy& policy,
                          Credentials_Manager& creds,
                          const Public_Key* server_public_key,
                          const std::string& hostname,
                          RandomNumberGenerator& rng);

      Client_Key_Exchange(const std::vector<byte>& buf,
                          const Handshake_State& state,
                          const Private_Key* server_rsa_kex_key,
                          Credentials_Manager& creds,
                          const Policy& policy,
                          RandomNumberGenerator& rng);

   private:
      std::vector<byte> serialize() const override
         { return m_key_material; }

      std::vector<byte> m_key_material;
      secure_vector<byte> m_pre_master;
   };

/**
* Certificate Message
*/
class Certificate final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return CERTIFICATE; }
      const std::vector<X509_Certificate>& cert_chain() const { return m_certs; }

      size_t count() const { return m_certs.size(); }
      bool empty() const { return m_certs.empty(); }

      Certificate(Handshake_IO& io,
                  Handshake_Hash& hash,
                  const std::vector<X509_Certificate>& certs);

      explicit Certificate(const std::vector<byte>& buf);
   private:
      std::vector<byte> serialize() const override;

      std::vector<X509_Certificate> m_certs;
   };

/**
* Certificate Request Message
*/
class Certificate_Req final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return CERTIFICATE_REQUEST; }

      const std::vector<std::string>& acceptable_cert_types() const
         { return m_cert_key_types; }

      std::vector<X509_DN> acceptable_CAs() const { return m_names; }

      std::vector<std::pair<std::string, std::string> > supported_algos() const
         { return m_supported_algos; }

      Certificate_Req(Handshake_IO& io,
                      Handshake_Hash& hash,
                      const Policy& policy,
                      const std::vector<X509_DN>& allowed_cas,
                      Protocol_Version version);

      Certificate_Req(const std::vector<byte>& buf,
                      Protocol_Version version);
   private:
      std::vector<byte> serialize() const override;

      std::vector<X509_DN> m_names;
      std::vector<std::string> m_cert_key_types;

      std::vector<std::pair<std::string, std::string> > m_supported_algos;
   };

/**
* Certificate Verify Message
*/
class Certificate_Verify final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return CERTIFICATE_VERIFY; }

      /**
      * Check the signature on a certificate verify message
      * @param cert the purported certificate
      * @param state the handshake state
      */
      bool verify(const X509_Certificate& cert,
                  const Handshake_State& state,
                  const Policy& policy) const;

      Certificate_Verify(Handshake_IO& io,
                         Handshake_State& state,
                         const Policy& policy,
                         RandomNumberGenerator& rng,
                         const Private_Key* key);

      Certificate_Verify(const std::vector<byte>& buf,
                         Protocol_Version version);
   private:
      std::vector<byte> serialize() const override;

      std::string m_sig_algo; // sig algo used to create signature
      std::string m_hash_algo; // hash used to create signature
      std::vector<byte> m_signature;
   };

/**
* Finished Message
*/
class Finished final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return FINISHED; }

      std::vector<byte> verify_data() const
         { return m_verification_data; }

      bool verify(const Handshake_State& state,
                  Connection_Side side) const;

      Finished(Handshake_IO& io,
               Handshake_State& state,
               Connection_Side side);

      explicit Finished(const std::vector<byte>& buf);
   private:
      std::vector<byte> serialize() const override;

      std::vector<byte> m_verification_data;
   };

/**
* Hello Request Message
*/
class Hello_Request final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return HELLO_REQUEST; }

      explicit Hello_Request(Handshake_IO& io);
      explicit Hello_Request(const std::vector<byte>& buf);
   private:
      std::vector<byte> serialize() const override;
   };

/**
* Server Key Exchange Message
*/
class Server_Key_Exchange final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return SERVER_KEX; }

      const std::vector<byte>& params() const { return m_params; }

      bool verify(const Public_Key& server_key,
                  const Handshake_State& state,
                  const Policy& policy) const;

      // Only valid for certain kex types
      const Private_Key& server_kex_key() const;

#if defined(BOTAN_HAS_SRP6)
      // Only valid for SRP negotiation
      SRP6_Server_Session& server_srp_params() const
         {
         BOTAN_ASSERT_NONNULL(m_srp_params);
         return *m_srp_params;
         }
#endif

      Server_Key_Exchange(Handshake_IO& io,
                          Handshake_State& state,
                          const Policy& policy,
                          Credentials_Manager& creds,
                          RandomNumberGenerator& rng,
                          const Private_Key* signing_key = nullptr);

      Server_Key_Exchange(const std::vector<byte>& buf,
                          const std::string& kex_alg,
                          const std::string& sig_alg,
                          Protocol_Version version);

      ~Server_Key_Exchange();
   private:
      std::vector<byte> serialize() const override;

#if defined(BOTAN_HAS_SRP6)
      std::unique_ptr<SRP6_Server_Session> m_srp_params;
#endif
      std::unique_ptr<Private_Key> m_kex_key;

      std::vector<byte> m_params;

      std::string m_sig_algo; // sig algo used to create signature
      std::string m_hash_algo; // hash used to create signature
      std::vector<byte> m_signature;
   };

/**
* Server Hello Done Message
*/
class Server_Hello_Done final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return SERVER_HELLO_DONE; }

      Server_Hello_Done(Handshake_IO& io, Handshake_Hash& hash);
      explicit Server_Hello_Done(const std::vector<byte>& buf);
   private:
      std::vector<byte> serialize() const override;
   };

/**
* New Session Ticket Message
*/
class New_Session_Ticket final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return NEW_SESSION_TICKET; }

      u32bit ticket_lifetime_hint() const { return m_ticket_lifetime_hint; }
      const std::vector<byte>& ticket() const { return m_ticket; }

      New_Session_Ticket(Handshake_IO& io,
                         Handshake_Hash& hash,
                         const std::vector<byte>& ticket,
                         u32bit lifetime);

      New_Session_Ticket(Handshake_IO& io,
                         Handshake_Hash& hash);

      explicit New_Session_Ticket(const std::vector<byte>& buf);
   private:
      std::vector<byte> serialize() const override;

      u32bit m_ticket_lifetime_hint = 0;
      std::vector<byte> m_ticket;
   };

/**
* Change Cipher Spec
*/
class Change_Cipher_Spec final : public Handshake_Message
   {
   public:
      Handshake_Type type() const override { return HANDSHAKE_CCS; }

      std::vector<byte> serialize() const override
         { return std::vector<byte>(1, 1); }
   };

}

}


namespace Botan {

namespace TLS {

/**
* Helper class for decoding TLS protocol messages
*/
class TLS_Data_Reader
   {
   public:
      TLS_Data_Reader(const char* type, const std::vector<byte>& buf_in) :
         m_typename(type), m_buf(buf_in), m_offset(0) {}

      void assert_done() const
         {
         if(has_remaining())
            throw decode_error("Extra bytes at end of message");
         }

      size_t read_so_far() const { return m_offset; }

      size_t remaining_bytes() const { return m_buf.size() - m_offset; }

      bool has_remaining() const { return (remaining_bytes() > 0); }

      std::vector<byte> get_remaining()
         {
         return std::vector<byte>(m_buf.begin() + m_offset, m_buf.end());
         }

      void discard_next(size_t bytes)
         {
         assert_at_least(bytes);
         m_offset += bytes;
         }

      u32bit get_u32bit()
         {
         assert_at_least(4);
         u32bit result = make_u32bit(m_buf[m_offset  ], m_buf[m_offset+1],
                                     m_buf[m_offset+2], m_buf[m_offset+3]);
         m_offset += 4;
         return result;
         }

      u16bit get_u16bit()
         {
         assert_at_least(2);
         u16bit result = make_u16bit(m_buf[m_offset], m_buf[m_offset+1]);
         m_offset += 2;
         return result;
         }

      byte get_byte()
         {
         assert_at_least(1);
         byte result = m_buf[m_offset];
         m_offset += 1;
         return result;
         }

      template<typename T, typename Container>
      Container get_elem(size_t num_elems)
         {
         assert_at_least(num_elems * sizeof(T));

         Container result(num_elems);

         for(size_t i = 0; i != num_elems; ++i)
            result[i] = load_be<T>(&m_buf[m_offset], i);

         m_offset += num_elems * sizeof(T);

         return result;
         }

      template<typename T>
      std::vector<T> get_range(size_t len_bytes,
                               size_t min_elems,
                               size_t max_elems)
         {
         const size_t num_elems =
            get_num_elems(len_bytes, sizeof(T), min_elems, max_elems);

         return get_elem<T, std::vector<T> >(num_elems);
         }

      template<typename T>
      std::vector<T> get_range_vector(size_t len_bytes,
                                      size_t min_elems,
                                      size_t max_elems)
         {
         const size_t num_elems =
            get_num_elems(len_bytes, sizeof(T), min_elems, max_elems);

         return get_elem<T, std::vector<T> >(num_elems);
         }

      std::string get_string(size_t len_bytes,
                             size_t min_bytes,
                             size_t max_bytes)
         {
         std::vector<byte> v =
            get_range_vector<byte>(len_bytes, min_bytes, max_bytes);

         return std::string(reinterpret_cast<char*>(v.data()), v.size());
         }

      template<typename T>
      std::vector<T> get_fixed(size_t size)
         {
         return get_elem<T, std::vector<T> >(size);
         }

   private:
      size_t get_length_field(size_t len_bytes)
         {
         assert_at_least(len_bytes);

         if(len_bytes == 1)
            return get_byte();
         else if(len_bytes == 2)
            return get_u16bit();

         throw decode_error("Bad length size");
         }

      size_t get_num_elems(size_t len_bytes,
                           size_t T_size,
                           size_t min_elems,
                           size_t max_elems)
         {
         const size_t byte_length = get_length_field(len_bytes);

         if(byte_length % T_size != 0)
            throw decode_error("Size isn't multiple of T");

         const size_t num_elems = byte_length / T_size;

         if(num_elems < min_elems || num_elems > max_elems)
            throw decode_error("Length field outside parameters");

         return num_elems;
         }

      void assert_at_least(size_t n) const
         {
         if(m_buf.size() - m_offset < n)
            throw decode_error("Expected " + std::to_string(n) +
                               " bytes remaining, only " +
                               std::to_string(m_buf.size()-m_offset) +
                               " left");
         }

      Decoding_Error decode_error(const std::string& why) const
         {
         return Decoding_Error("Invalid " + std::string(m_typename) + ": " + why);
         }

      const char* m_typename;
      const std::vector<byte>& m_buf;
      size_t m_offset;
   };

/**
* Helper function for encoding length-tagged vectors
*/
template<typename T, typename Alloc>
void append_tls_length_value(std::vector<byte, Alloc>& buf,
                             const T* vals,
                             size_t vals_size,
                             size_t tag_size)
   {
   const size_t T_size = sizeof(T);
   const size_t val_bytes = T_size * vals_size;

   if(tag_size != 1 && tag_size != 2)
      throw Invalid_Argument("append_tls_length_value: invalid tag size");

   if((tag_size == 1 && val_bytes > 255) ||
      (tag_size == 2 && val_bytes > 65535))
      throw Invalid_Argument("append_tls_length_value: value too large");

   for(size_t i = 0; i != tag_size; ++i)
      buf.push_back(get_byte(sizeof(val_bytes)-tag_size+i, val_bytes));

   for(size_t i = 0; i != vals_size; ++i)
      for(size_t j = 0; j != T_size; ++j)
         buf.push_back(get_byte(j, vals[i]));
   }

template<typename T, typename Alloc, typename Alloc2>
void append_tls_length_value(std::vector<byte, Alloc>& buf,
                             const std::vector<T, Alloc2>& vals,
                             size_t tag_size)
   {
   append_tls_length_value(buf, vals.data(), vals.size(), tag_size);
   }

template<typename Alloc>
void append_tls_length_value(std::vector<byte, Alloc>& buf,
                             const std::string& str,
                             size_t tag_size)
   {
   append_tls_length_value(buf,
                           reinterpret_cast<const byte*>(str.data()),
                           str.size(),
                           tag_size);
   }

}

}


namespace Botan {

namespace TLS {

class Ciphersuite;
class Session_Keys;

class Connection_Sequence_Numbers;

/**
* TLS Cipher State
*/
class Connection_Cipher_State
   {
   public:
      /**
      * Initialize a new cipher state
      */
      Connection_Cipher_State(Protocol_Version version,
                              Connection_Side which_side,
                              bool is_our_side,
                              const Ciphersuite& suite,
                              const Session_Keys& keys);

      AEAD_Mode* aead() { return m_aead.get(); }

      std::vector<byte> aead_nonce(u64bit seq);

      std::vector<byte> aead_nonce(const byte record[], size_t record_len, u64bit seq);

      std::vector<byte> format_ad(u64bit seq, byte type,
                                  Protocol_Version version,
                                  u16bit ptext_length);

      BlockCipher* block_cipher() { return m_block_cipher.get(); }

      MessageAuthenticationCode* mac() { return m_mac.get(); }

      secure_vector<byte>& cbc_state() { return m_block_cipher_cbc_state; }

      size_t block_size() const { return m_block_size; }

      size_t mac_size() const { return m_mac->output_length(); }

      size_t iv_size() const { return m_iv_size; }

      size_t nonce_bytes_from_record() const { return m_nonce_bytes_from_record; }

      size_t nonce_bytes_from_handshake() const { return m_nonce_bytes_from_handshake; }

      bool cbc_without_explicit_iv() const
         { return (m_block_size > 0) && (m_iv_size == 0); }

      std::chrono::seconds age() const
         {
         return std::chrono::duration_cast<std::chrono::seconds>(
            std::chrono::system_clock::now() - m_start_time);
         }

   private:
      std::chrono::system_clock::time_point m_start_time;
      std::unique_ptr<BlockCipher> m_block_cipher;
      secure_vector<byte> m_block_cipher_cbc_state;
      std::unique_ptr<MessageAuthenticationCode> m_mac;

      std::unique_ptr<AEAD_Mode> m_aead;
      std::vector<byte> m_nonce;

      size_t m_block_size = 0;
      size_t m_nonce_bytes_from_handshake;
      size_t m_nonce_bytes_from_record;
      size_t m_iv_size = 0;
   };

/**
* Create a TLS record
* @param write_buffer the output record is placed here
* @param msg_type is the type of the message (handshake, alert, ...)
* @param msg is the plaintext message
* @param msg_length is the length of msg
* @param msg_sequence is the sequence number
* @param version is the protocol version
* @param cipherstate is the writing cipher state
* @param rng is a random number generator
* @return number of bytes written to write_buffer
*/
void write_record(secure_vector<byte>& write_buffer,
                  byte msg_type, const byte msg[], size_t msg_length,
                  Protocol_Version version,
                  u64bit msg_sequence,
                  Connection_Cipher_State* cipherstate,
                  RandomNumberGenerator& rng);

// epoch -> cipher state
typedef std::function<std::shared_ptr<Connection_Cipher_State> (u16bit)> get_cipherstate_fn;

/**
* Decode a TLS record
* @return zero if full message, else number of bytes still needed
*/
size_t read_record(secure_vector<byte>& read_buffer,
                   const byte input[],
                   size_t input_length,
                   bool is_datagram,
                   size_t& input_consumed,
                   secure_vector<byte>& record,
                   u64bit* record_sequence,
                   Protocol_Version* record_version,
                   Record_Type* record_type,
                   Connection_Sequence_Numbers* sequence_numbers,
                   get_cipherstate_fn get_cipherstate);

}

}


namespace Botan {

namespace TLS {

class Connection_Sequence_Numbers
   {
   public:
      virtual ~Connection_Sequence_Numbers() {}

      virtual void new_read_cipher_state() = 0;
      virtual void new_write_cipher_state() = 0;

      virtual u16bit current_read_epoch() const = 0;
      virtual u16bit current_write_epoch() const = 0;

      virtual u64bit next_write_sequence(u16bit) = 0;
      virtual u64bit next_read_sequence() = 0;

      virtual bool already_seen(u64bit seq) const = 0;
      virtual void read_accept(u64bit seq) = 0;
   };

class Stream_Sequence_Numbers final : public Connection_Sequence_Numbers
   {
   public:
      void new_read_cipher_state() override { m_read_seq_no = 0; m_read_epoch += 1; }
      void new_write_cipher_state() override { m_write_seq_no = 0; m_write_epoch += 1; }

      u16bit current_read_epoch() const override { return m_read_epoch; }
      u16bit current_write_epoch() const override { return m_write_epoch; }

      u64bit next_write_sequence(u16bit) override { return m_write_seq_no++; }
      u64bit next_read_sequence() override { return m_read_seq_no; }

      bool already_seen(u64bit) const override { return false; }
      void read_accept(u64bit) override { m_read_seq_no++; }
   private:
      u64bit m_write_seq_no = 0;
      u64bit m_read_seq_no = 0;
      u16bit m_read_epoch = 0;
      u16bit m_write_epoch = 0;
   };

class Datagram_Sequence_Numbers final : public Connection_Sequence_Numbers
   {
   public:
      Datagram_Sequence_Numbers() { m_write_seqs[0] = 0; }

      void new_read_cipher_state() override { m_read_epoch += 1; }

      void new_write_cipher_state() override
         {
         m_write_epoch += 1;
         m_write_seqs[m_write_epoch] = 0;
         }

      u16bit current_read_epoch() const override { return m_read_epoch; }
      u16bit current_write_epoch() const override { return m_write_epoch; }

      u64bit next_write_sequence(u16bit epoch) override
         {
         auto i = m_write_seqs.find(epoch);
         BOTAN_ASSERT(i != m_write_seqs.end(), "Found epoch");
         return (static_cast<u64bit>(epoch) << 48) | i->second++;
         }

      u64bit next_read_sequence() override
         {
         throw Exception("DTLS uses explicit sequence numbers");
         }

      bool already_seen(u64bit sequence) const override
         {
         const size_t window_size = sizeof(m_window_bits) * 8;

         if(sequence > m_window_highest)
            return false;

         const u64bit offset = m_window_highest - sequence;

         if(offset >= window_size)
            return true; // really old?

         return (((m_window_bits >> offset) & 1) == 1);
         }

      void read_accept(u64bit sequence) override
         {
         const size_t window_size = sizeof(m_window_bits) * 8;

         if(sequence > m_window_highest)
            {
            const size_t offset = sequence - m_window_highest;
            m_window_highest += offset;

            if(offset >= window_size)
               m_window_bits = 0;
            else
               m_window_bits <<= offset;

            m_window_bits |= 0x01;
            }
         else
            {
            const u64bit offset = m_window_highest - sequence;
            m_window_bits |= (static_cast<u64bit>(1) << offset);
            }
         }

   private:
      std::map<u16bit, u64bit> m_write_seqs;
      u16bit m_write_epoch = 0;
      u16bit m_read_epoch = 0;
      u64bit m_window_highest = 0;
      u64bit m_window_bits = 0;
   };

}

}


namespace Botan {

/**
* Entropy source for generic Unix. Runs various programs trying to
* gather data hard for a remote attacker to guess. Probably not too
* effective against local attackers as they can sample from the same
* distribution.
*/
class Unix_EntropySource final : public Entropy_Source
   {
   public:
      std::string name() const override { return "unix_procs"; }

      void poll(Entropy_Accumulator& accum) override;

      /**
      * @param trusted_paths is a list of directories that are assumed
      *        to contain only 'safe' binaries. If an attacker can write
      *        an executable to one of these directories then we will
      *        run arbitrary code.
      */
      Unix_EntropySource(const std::vector<std::string>& trusted_paths,
                         size_t concurrent_processes = 0);
   private:
      static std::vector<std::vector<std::string>> get_default_sources();

      class Unix_Process
         {
         public:
            int fd() const { return m_fd; }

            void spawn(const std::vector<std::string>& args);
            void shutdown();

            Unix_Process() {}

            Unix_Process(const std::vector<std::string>& args) { spawn(args); }

            ~Unix_Process() { shutdown(); }

            Unix_Process(Unix_Process&& other)
               {
               std::swap(m_fd, other.m_fd);
               std::swap(m_pid, other.m_pid);
               }

            Unix_Process(const Unix_Process&) = delete;
            Unix_Process& operator=(const Unix_Process&) = delete;
         private:
            int m_fd = -1;
            int m_pid = -1;
         };

      const std::vector<std::string>& next_source();

      std::mutex m_mutex;
      const std::vector<std::string> m_trusted_paths;
      const size_t m_concurrent;

      std::vector<std::vector<std::string>> m_sources;
      size_t m_sources_idx = 0;

      std::vector<Unix_Process> m_procs;
      secure_vector<byte> m_buf;
   };

class UnixProcessInfo_EntropySource final : public Entropy_Source
   {
   public:
      std::string name() const override { return "proc_info"; }

      void poll(Entropy_Accumulator& accum) override;
   };

}


#endif
