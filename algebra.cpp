/*
 * Scientific Calculator — C++
 * ---------------------------------------------------------
 * A from-scratch expression evaluator (no eval()-style shortcuts).
 * Implements a recursive-descent parser respecting standard
 * mathematical operator precedence and associativity.
 *
 * Supports:
 *   - Operators: + - * / ^ % (with correct precedence)
 *   - Unary minus/plus, parentheses, nested expressions
 *   - Functions: sin cos tan asin acos atan sinh cosh tanh
 *                log ln sqrt cbrt exp abs fact
 *   - Constants: pi, e
 *   - Memory: M+  M-  MR  MC
 *   - Clear, specific error messages (syntax, domain, division by zero)
 *
 * Grammar (highest to lowest precedence):
 *   primary    := NUMBER | CONSTANT | FUNCTION '(' expression ')'
 *              |  '(' expression ')' | ('-' | '+') unary
 *   power      := primary ('^' unary)?          (right-associative)
 *   term       := power (('*' | '/' | '%') power)*
 *   expression := term (('+' | '-') term)*
 *
 * Compile:  g++ -std=c++17 -Wall -Wextra -O2 scientific_calculator.cpp -o calc
 * Run:      ./calc
 */

#include <cctype>
#include <cmath>
#include <functional>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>

// Defined manually instead of relying on M_PI / M_E, which are POSIX
// extensions and are NOT guaranteed available in strict ISO C++ mode
// (this is exactly what MinGW/g++ on Windows enforces by default).
constexpr double PI_CONST = 3.14159265358979323846;
constexpr double E_CONST = 2.71828182845904523536;

// ============================================================
// Custom exception — precise, human-readable error reporting
// ============================================================
class CalculatorError : public std::runtime_error {
   public:
    explicit CalculatorError(const std::string& message) : std::runtime_error(message) {
    }
};

// ============================================================
// Tokenizer + Recursive-Descent Parser + Evaluator (combined
// for a compact single-pass design — common in small interpreters)
// ============================================================
class ExpressionParser {
   public:
    explicit ExpressionParser(const std::string& expression) : text_(expression), pos_(0) {
    }

    double evaluate() {
        skipWhitespace();
        if (pos_ >= text_.size()) {
            throw CalculatorError("Empty expression.");
        }
        double result = parseExpression();
        skipWhitespace();
        if (pos_ != text_.size()) {
            throw CalculatorError("Unexpected character at position " + std::to_string(pos_) +
                                  ": '" + text_[pos_] + "'");
        }
        return result;
    }

   private:
    const std::string& text_;
    size_t pos_;

    // ---- low-level character helpers ----
    void skipWhitespace() {
        while (pos_ < text_.size() && std::isspace(static_cast<unsigned char>(text_[pos_]))) {
            ++pos_;
        }
    }

    char peek() {
        skipWhitespace();
        return pos_ < text_.size() ? text_[pos_] : '\0';
    }

    char consume() {
        skipWhitespace();
        if (pos_ >= text_.size()) {
            throw CalculatorError("Unexpected end of expression.");
        }
        return text_[pos_++];
    }

    bool match(char expected) {
        if (peek() == expected) {
            ++pos_;  // skipWhitespace() in peek() already advanced pos_ to the char
            return true;
        }
        return false;
    }

    // ---- grammar rules, highest level first ----

    // expression := term (('+' | '-') term)*
    double parseExpression() {
        double value = parseTerm();
        while (true) {
            char op = peek();
            if (op == '+') {
                ++pos_;
                value += parseTerm();
            } else if (op == '-') {
                ++pos_;
                value -= parseTerm();
            } else
                break;
        }
        return value;
    }

    // term := power (('*' | '/' | '%') power)*
    double parseTerm() {
        double value = parsePower();
        while (true) {
            char op = peek();
            if (op == '*') {
                ++pos_;
                value *= parsePower();
            } else if (op == '/') {
                ++pos_;
                double divisor = parsePower();
                if (divisor == 0.0) throw CalculatorError("Division by zero.");
                value /= divisor;
            } else if (op == '%') {
                ++pos_;
                double divisor = parsePower();
                if (divisor == 0.0) throw CalculatorError("Modulo by zero.");
                value = std::fmod(value, divisor);
            } else {
                break;
            }
        }
        return value;
    }

    // power := unary ('^' unary)?   -- right-associative: 2^3^2 = 2^(3^2)
    double parsePower() {
        double base = parseUnary();
        if (peek() == '^') {
            ++pos_;
            double exponent = parsePower();  // recurse for right-associativity
            return std::pow(base, exponent);
        }
        return base;
    }

    // unary := ('-' | '+')? primary
    double parseUnary() {
        if (peek() == '-') {
            ++pos_;
            return -parseUnary();
        }
        if (peek() == '+') {
            ++pos_;
            return parseUnary();
        }
        return parsePrimary();
    }

    // primary := NUMBER | CONSTANT | FUNCTION '(' expr ')' | '(' expr ')'
    double parsePrimary() {
        char c = peek();

        if (c == '(') {
            ++pos_;
            double value = parseExpression();
            if (!match(')')) throw CalculatorError("Expected closing ')'.");
            return value;
        }

        if (std::isdigit(static_cast<unsigned char>(c)) || c == '.') {
            return parseNumber();
        }

        if (std::isalpha(static_cast<unsigned char>(c))) {
            return parseIdentifier();  // function call or named constant
        }

        throw CalculatorError(std::string("Unexpected character '") + c + "'.");
    }

    double parseNumber() {
        size_t start = pos_;
        skipWhitespace();
        start = pos_;
        while (pos_ < text_.size() &&
               (std::isdigit(static_cast<unsigned char>(text_[pos_])) || text_[pos_] == '.')) {
            ++pos_;
        }
        if (start == pos_) throw CalculatorError("Malformed number.");
        try {
            return std::stod(text_.substr(start, pos_ - start));
        } catch (const std::exception&) {
            throw CalculatorError("Malformed number literal.");
        }
    }

    std::string parseWord() {
        skipWhitespace();
        size_t start = pos_;
        while (pos_ < text_.size() && std::isalpha(static_cast<unsigned char>(text_[pos_]))) {
            ++pos_;
        }
        return text_.substr(start, pos_ - start);
    }

    double parseIdentifier() {
        std::string word = parseWord();

        // Named constants
        if (word == "pi") return PI_CONST;
        if (word == "e") return E_CONST;

        // Function call: NAME '(' expr ')'
        if (peek() != '(') {
            throw CalculatorError("Unknown identifier '" + word + "'.");
        }
        ++pos_;  // consume '('
        double argument = parseExpression();
        if (!match(')')) throw CalculatorError("Expected ')' after argument to '" + word + "'.");

        return applyFunction(word, argument);
    }

    double applyFunction(const std::string& name, double x) {
        static const std::unordered_map<std::string, std::function<double(double)>> functions = {
            {"sin", [](double v) { return std::sin(v); }},
            {"cos", [](double v) { return std::cos(v); }},
            {"tan", [](double v) { return std::tan(v); }},
            {"asin",
             [](double v) {
                 if (v < -1.0 || v > 1.0) throw CalculatorError("asin domain is [-1, 1].");
                 return std::asin(v);
             }},
            {"acos",
             [](double v) {
                 if (v < -1.0 || v > 1.0) throw CalculatorError("acos domain is [-1, 1].");
                 return std::acos(v);
             }},
            {"atan", [](double v) { return std::atan(v); }},
            {"sinh", [](double v) { return std::sinh(v); }},
            {"cosh", [](double v) { return std::cosh(v); }},
            {"tanh", [](double v) { return std::tanh(v); }},
            {"sqrt",
             [](double v) {
                 if (v < 0.0) throw CalculatorError("Cannot take sqrt of a negative number.");
                 return std::sqrt(v);
             }},
            {"cbrt", [](double v) { return std::cbrt(v); }},
            {"log",
             [](double v) {
                 if (v <= 0.0) throw CalculatorError("log domain is (0, infinity).");
                 return std::log10(v);
             }},
            {"ln",
             [](double v) {
                 if (v <= 0.0) throw CalculatorError("ln domain is (0, infinity).");
                 return std::log(v);
             }},
            {"exp", [](double v) { return std::exp(v); }},
            {"abs", [](double v) { return std::fabs(v); }},
            {"fact",
             [](double v) {
                 if (v < 0.0 || v != std::floor(v)) {
                     throw CalculatorError("fact() requires a non-negative integer.");
                 }
                 double result = 1.0;
                 for (int i = 2; i <= static_cast<int>(v); ++i) result *= i;
                 return result;
             }},
        };

        auto it = functions.find(name);
        if (it == functions.end()) {
            throw CalculatorError("Unknown function '" + name + "'.");
        }
        return it->second(x);
    }
};

// ============================================================
// Memory unit — simple stateful register, common on real calculators
// ============================================================
class MemoryRegister {
   public:
    void add(double value) {
        stored_ += value;
    }
    void subtract(double value) {
        stored_ -= value;
    }
    double recall() const {
        return stored_;
    }
    void clear() {
        stored_ = 0.0;
    }

   private:
    double stored_ = 0.0;
};

// ============================================================
// REPL — the interactive command loop
// ============================================================
void printHelp() {
    std::cout << "\nScientific Calculator — commands:\n"
                 "  <expression>     evaluate a math expression, e.g. sin(pi/2) + 2^3\n"
                 "  M+                add last result to memory\n"
                 "  M-                subtract last result from memory\n"
                 "  MR                recall memory\n"
                 "  MC                clear memory\n"
                 "  help              show this message\n"
                 "  exit              quit\n\n"
                 "Supported: + - * / ^ % , parentheses, unary +/-\n"
                 "Functions: sin cos tan asin acos atan sinh cosh tanh\n"
                 "           sqrt cbrt log ln exp abs fact\n"
                 "Constants: pi, e\n\n";
}

int main() {
    MemoryRegister memory;
    double lastResult = 0.0;
    std::string line;

    std::cout << "Scientific Calculator (type 'help' for commands, 'exit' to quit)\n";

    while (true) {
        std::cout << "> ";
        if (!std::getline(std::cin, line)) break;

        // Trim
        size_t begin = line.find_first_not_of(" \t");
        if (begin == std::string::npos) continue;
        size_t end = line.find_last_not_of(" \t");
        line = line.substr(begin, end - begin + 1);

        if (line == "exit" || line == "quit") break;
        if (line == "help") {
            printHelp();
            continue;
        }
        if (line == "M+") {
            memory.add(lastResult);
            std::cout << "Memory: " << memory.recall() << '\n';
            continue;
        }
        if (line == "M-") {
            memory.subtract(lastResult);
            std::cout << "Memory: " << memory.recall() << '\n';
            continue;
        }
        if (line == "MR") {
            std::cout << "Memory: " << memory.recall() << '\n';
            continue;
        }
        if (line == "MC") {
            memory.clear();
            std::cout << "Memory cleared.\n";
            continue;
        }

        try {
            ExpressionParser parser(line);
            lastResult = parser.evaluate();
            std::cout << "= " << lastResult << '\n';
        } catch (const CalculatorError& e) {
            std::cout << "Error: " << e.what() << '\n';
        } catch (const std::exception& e) {
            std::cout << "Unexpected error: " << e.what() << '\n';
        }
    }

    std::cout << "Goodbye.\n";
    return 0;
}
