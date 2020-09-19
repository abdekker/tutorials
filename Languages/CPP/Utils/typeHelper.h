#pragma once
#include <string_view>

// Header-only helper class for investigating types
class DataTypeHelper
{
public:
    // Constructor / destructor
    DataTypeHelper() {}
    ~DataTypeHelper() {}

    template<class T>
    std::string name_typeid(T t) {
        return typeid(t).name();
    }

    template <typename T>
    constexpr std::string_view name__FUNCSIG__(T)
    {
        std::string_view name, prefix, suffix;
#ifdef __clang__
        name = __PRETTY_FUNCTION__;
        prefix = "std::string_view type_name() [T = ";
        //suffix = "]";
#elif defined(__GNUC__)
        name = __PRETTY_FUNCTION__;
        prefix = "constexpr std::string_view type_name() [with T = ";
        //suffix = "; std::string_view = std::basic_string_view<char>]";
#elif defined(_MSC_VER)
        name = __FUNCSIG__;
        prefix = "class std::basic_string_view<char,struct std::char_traits<char> > __thiscall DataTypeHelper::";
        //prefix = "class std::basic_string_view<char,struct std::char_traits<char> > __thiscall DataTypeHelper::name__FUNCSIG__<char>(char)"
        //suffix = ">(void)";
#endif

        // Remove the prefix (which is the same for all types). Comment this out to see the full
        // function signature from "__PRETTY_FUNCTION__" or "__FUNCSIG__".
        name.remove_prefix(prefix.size());
        //name.remove_suffix(suffix.size());
        return name;
    }
};
