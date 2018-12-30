#include <iostream>
#include <unordered_map>

int main()
{
  int limit = 36000000;
  std::unordered_map<int, int> houses;
  
  for (int elf = 1;; elf++) {
    int p = elf * 11;

    for (int house = elf; house <= elf * 50; house += elf) {      
      auto where = houses.find(house);
      if (where == houses.end()) {
        houses[house] = p;
      } else {
        where->second += p;
      }
    }
    
    if (houses[elf] >= limit) {
      std::cout << "Found house: " << elf << "\n";
      break;
    } else {
      houses.erase(elf);
    }
  }
  
  return 0;
}
