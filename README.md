# 🧩 AnagramsMC – Anagram Detection with Wolfram Language

This project focuses on **generating and detecting valid anagrams** of words or short phrases using **dictionary-based matching**.  
It is implemented in **Wolfram Language (Mathematica)** and supports both **Italian** and **English** vocabularies.

The goal is to explore combinatorial word generation while filtering results through real-language dictionaries to retain only meaningful anagrams.

---

## 📌 Project Highlights

### 🔍 Problem Statement
Anagrams are easy to generate combinatorially, but **hard to validate linguistically**.  
The main challenges include:

✔ Exponential growth of letter permutations  
✔ Efficient filtering against large dictionaries  
✔ Multi-language support  
✔ Maintaining readable and reusable logic  

---

## 🛠️ Our Approach
The project separates **logic** and **interface**, making the code modular and easy to extend.

- **Dictionary-based validation** to keep only real words  
- **Language-agnostic logic**, with external dictionary files  
- **Wolfram Language** for symbolic manipulation and pattern matching  

---

### ⚙️ Key Steps

#### Dictionary Handling
- Load word lists from `.txt` files  
- Normalize and preprocess words  
- Support for **Italian** and **English** dictionaries  

#### Anagram Generation
- Generate permutations of input characters  
- Filter permutations using the selected dictionary  
- Return only valid anagrams  

#### User Interaction
- High-level interface functions for ease of use  
- Tutorial notebook with usage examples  

---
## 👥 Contributors 
Arianna Arruzzoli  
Benedetta Bottari  
Claudia Brunetti  
Milena Mazza 

## 📜 License: 
This project is licensed under the MIT License.


## 🎓 Academic Context
Academic Year **2023/2024** - University of Bologna

