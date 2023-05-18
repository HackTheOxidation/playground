#include <picoJSON/Content.hpp>
#include <picoJSON/Parser.hpp>
#include <string>
#include <iostream>

int main() {
  picoJSON::Parser parser("config.json");
  picoJSON::Content content = parser.getContent();

  std::string str("");
  content[str];
  return 0;
}
