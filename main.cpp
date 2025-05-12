#include <iostream>
#include <locale>
#include <codecvt>
#include <fcntl.h>
#include <io.h>

int main() {
    // Set the locale to support Unicode
    std::locale::global(std::locale(".UTF-8"));
    _setmode(_fileno(stdout), _O_U16TEXT);
    std::wcout << L"Hello, Unicode:  😊 こんにちは" << std::endl;
    return 0;
}