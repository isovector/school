#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <fstream>
using namespace std;

#define BIND(name) Hangman::__functor_##name

#define FUNCTOR(name, T)                        \
    private:                                    \
    struct __functor_##name {                   \
        Hangman *me;                            \
        T arg;                                  \
                                                \
        __functor_##name(Hangman *h)            \
            : me(h) { }                         \
                                                \
        __functor_##name(Hangman *h, T a)       \
            : me(h), arg(a) { }                 \
                                                \
        T operator()(T x) {                     \
            return me->__func_##name(x, arg);   \
        }                                       \
    };                                          \
                                                \
    friend struct __functor_##name;             \
    T __func_##name(T x, T arg)


template<class T, class P>
bool for_all(const T &input, P func) {
    return count_if(input.begin(), input.end(), func) == input.size();
}

template<class T, class P>
bool for_any(const T &input, P func) {
    return find_if(input.begin(), input.end(), func) != input.end();
}

template<class T, class P>
void walk(const T &input, T &output, P func) {
    output.resize(input.size());
    transform(input.begin(), input.end(), output.begin(), func);
}

template<typename C>
bool exists_in(const C &input, typename C::value_type value) {
    return find(input.begin(), input.end(), value) != input.end();
}

// END OF GENERIC MAGIC

class Hangman {
    friend ostream &operator <<(ostream &stream, Hangman hangman);
    vector<char> guesses_;
    string word_;
    string lower_;
    bool done_;
    int lives_;

public:
    Hangman(string word) : word_(word), done_(false), lives_(5) {
        walk(word, lower_, (int(*)(int))tolower);
    }
    
    bool over() const {
        return done_ || lives_ <= 0;
    }
    
    operator bool() const {
        return done_;
    }
    
    string word() const {
        return word_;
    }
    
    bool guess(string g) {
        if (g.size() == 1)
            return guess(g[0]);
        
        if (g != word_)
            lives_ = 0;
        else
            done_ = true;
        
        return true;
    }

    bool guess(char c) {
        c = tolower(c);

        if (exists_in(guesses_, c)) {
            cout << "You already guessed letter \"" << c << "\"." << endl;
            return false;
        }
        
        guesses_.push_back(c);
        
        if (!for_any(lower_, BIND(is_good_guess)(this, c))) {
            --lives_;
            return false;
        }
        
        // Apparently we don't want to autowin :(
        // done_ = for_all(word_, BIND(is_finished)(this));
        
        return true;
    }
    
    int size() const { return word_.length(); }
    
    
    FUNCTOR(is_good_guess, char) {
        return x == arg;
    }
    
    FUNCTOR(censor, char) {
        return exists_in(guesses_, tolower(x)) ? x : '-';
    }
    
    FUNCTOR(is_finished, char) {
        return exists_in(guesses_, tolower(x));
    }
};


ostream &operator <<(ostream &stream, Hangman h) {
    string output;
    walk(h.word_, output, BIND(censor)(&h));
    
    stream << "Word: " << output << endl;
    stream << "Letters used:";
    for (size_t i = 0; i < h.guesses_.size(); i++)
        stream << ' ' << h.guesses_[i];
    
    return stream << endl << "You have " << h.lives_ << " " << (h.lives_ == 1 ? "life" : "lives") <<" left.";
}

int main(int argc, char *argv[]) {
    srand48(argc == 3 ? atoi(argv[2]) : 0);
    
    string input;
    vector<string> words;
    
    if (argc < 2) {
        cout << "Error: No input file specified." << endl;
        return 1;
    }
    
    ifstream listfile(argv[1]);
    
    if (!listfile) {
        cout << "Error: Could not open file \"" << argv[1] << "\"." << endl;
        return 1;
    }
    
    while (listfile) {
        listfile >> input;
        if (listfile && for_all(input, (int(*)(int))isalnum) && input.length() >= 6)
            words.push_back(input);
    }
    listfile.close();
    
    if (words.size() == 0) {
        cout << "Error: Pool of game words is empty." << endl;
        return 1;
    }
    
    ofstream gamewordsfile("gamewords");
    for (vector<string>::iterator it = words.begin(); it != words.end(); it++)
        gamewordsfile << *it << endl;
    gamewordsfile.close();
    
    while (cin) {
        Hangman h(words[lrand48() % words.size()]);
        while (!h.over()) {
            cout << h << endl;
            cout << "Next guess: ";
            cin >> input;
            
            if (!cin)
                return 0;
            
            h.guess(input);
        }
        
        if (h)
            cout << "You WIN!" << endl;
        else
            cout << "You LOSE!  The word was \"" << h.word() << "\"." << endl;
            
        cout << "Do you want to play again? [Y/N] ";
        cin >> input;
        
        if (tolower(input[0]) != 'y')
            break;
    }
        
    return 0;
}