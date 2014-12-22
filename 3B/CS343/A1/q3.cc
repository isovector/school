#include <uC++.h>
#include <iostream>
#include <fstream>
using namespace std;

_Coroutine Utf8 {
  public:
    struct Match {
        unsigned int unicode;
        Match(unsigned int unicode) : unicode(unicode) {}
    };
    struct Error {};
    
    Utf8() { resume(); }
    
    void next(unsigned char c) {
        ch = c;
        resume();
    }
        
  private:
    unsigned char ch;           // the last input character
    unsigned int consumedBytes; // bytes parsed so far
  
    void outputPreamble() {
        cout << hex << "0x" << consumedBytes << " : ";
    }
  
    void suspendAndConsume() { 
        suspend(); 
        consume();
    }
    
    // keeps track of bytes parsed for the current line
    void consume() {
        consumedBytes <<= 8; 
        consumedBytes |= ch; 
    }
    
    // eats bytes after a match until the next newline
    void consumeExtra() {
        // early exit if last matched character was a newline
        // special case when a unicode sequence ends early
        if (ch == '\n') {
            return;
        }
        
        bool hasExtra = false;
        
        for (;;) {
            suspend();
            
            // if the next character is a newline, there are no
            // extra bytes
            if (ch == '\n') {
                break;
            }
            
            if (!hasExtra) {
                hasExtra = true;
                cout << ". Extra characters 0x";
            }
            
            cout << hex << (int)ch;
        }
    }
    
    // consumes bytes one at a time, parsing the data stream into
    // utf8 characters, expecting one valid utf8 code per line. this
    // coroutine need not be reconstructed after every successful or
    // unsuccessful match.
    //
    // this coroutine does not use the sample code provided in the
    // assignment, as it is generally considered poor form to use
    // unions, and the bitwise operator approach used instead precludes
    // the necessity of endianness.
    void main() {
        for (;;) {
            try {
                // clear the bytes thus-far parsed
                consumedBytes = 0;
                
                suspendAndConsume();
                if (ch == '\n') {
                    // special case if first character of the line is a
                    // newline
                    cout << " : Warning! Blank line." << endl;
                    continue;
                }
                
                // if top-most bit is 0, this is regular ascii data
                int checkMask = 1 << 7;
                if (!(ch & checkMask)) {
                    throw Match(ch);
                }
                
                // ensure second highest bit is 0
                unsigned int invalidHeaderByteFilter = ~(1 << 6);
                if ((ch & invalidHeaderByteFilter) == ch) {
                    throw Error();
                }

                size_t subsequentBytes = 0;

                // determine how many 1s we have before the first 0.
                // this number is the total length of the unicode sequence
                checkMask >>= 1;
                for (; ch & checkMask; checkMask >>= 1) {
                    subsequentBytes++;
                    
                    if (subsequentBytes > 3) {
                        // no utf8 sequence can have more than 4 bytes
                        throw Error();
                    }
                }
                
                // calculate the smallest int necessary to correctly be using
                // this encoding. we will check later to ensure that our parsed
                // data is at least this big.
                size_t smallestBitInRange = 6 + 5 * (subsequentBytes - 1);
                const unsigned int smallestIntInRange = 1 << smallestBitInRange;
                
                // subtracting 1 from the last inspected mask bit gives us
                // a mask of the remaining bits in the byte
                const int dataMask = checkMask - 1;
                unsigned int utf8 = ch & dataMask;
                
                // iterate through our remaining bytes
                for (; subsequentBytes > 0; --subsequentBytes) {
                    suspend();
                    
                    if (ch == '\n') {
                        // sequence ends early
                        throw Error();
                    } else {
                        // continue tracking our consumed bytes (usually this is
                        // handled by suspendAndConsume())
                        consume();
                    }
                    
                    // ensure next byte is of the form 110xxxxx
                    const int subsequentCheckMask = (1 << 7);
                    const int subsequentCheckFilter = ~(1 << 6);
                    if (!(ch & subsequentCheckMask) || !((ch & subsequentCheckFilter) == ch)) {
                        throw Error();
                    }

                    // allocate space for the new utf8 data
                    utf8 <<= 6;
                    
                    // append the new utf8 data to the total
                    const int subsequentDataMask = (1 << 6) - 1;
                    utf8 |= ch & subsequentDataMask;
                }
                
                // check to see if the character has been encoded correctly (ie
                // using as few bytes as possible)
                if (utf8 < smallestIntInRange) {
                    throw Error();
                }
                
                throw Match(utf8);
            }
            
            // the current line produced a match
            catch (Match m) {
                outputPreamble();
                cout << "valid 0x" << hex << m.unicode;
                consumeExtra();
            }
            
            // the current line produced an error
            catch (Error e) {
                outputPreamble();
                cout << "invalid";
                consumeExtra();
            }
            
            cout << endl;
        }
    }
};

void usage( char *argv[] ) {
    cerr << "Usage: " << argv[0]
	 << " [ input-file ]" << endl;
}

void uMain::main() {
    Utf8 utf;
    
#if 0
    char bytes[] = {
        0x23, '\n',					// valid
        0x23, 0x23, '\n',				// valid, extra byte
        0xd7, 0x90, '\n',				// valid
        0xd7, '\n',					// invalid, missing a byte
        0xc2, 0xA3, '\n',				// valid
        '\n',						// empty line
        0xb0, '\n',					// invalid, value too large
        0xe0, 0xe3, '\n',				// invalid, second byte wrong, missing a byte
        0xe9, 0x80, 0x80, '\n',				// valid
        0xe9, 0x80, 0x80, 0xff, 0xf8, '\n',		// valid, extra 2 bytes
        0xe0, 0x93, 0x90, '\n',				// invalid, value encoded in wrong range
        0xff, 0x9A, 0x84, '\n',				// invalid, first byte, extra 2 bytes 
        0xf0, 0x90, 0x89, '\n',				// invalid, missing byte
        0xf0, 0x90, 0x89, 0x80, '\n',			// valid
        0x01, '\n',					// valid
    };
    
    for (size_t i = 0; i < (sizeof(bytes) / sizeof(bytes[0])); ++i) {
        utf.next(bytes[i]);
    }
#else

    // from https://www.student.cs.uwaterloo.ca/~cs343/examples/uIO.cc
    istream *infile = &cin;

    if (argc > 2) {
        usage(argv);
        return;
    }
    else if (argc == 2) {
        try {
            infile = new ifstream( argv[1] );
        } catch( uFile::Failure ) {
            cerr << "Error! Could not open input file \"" << argv[1] << "\"" << endl;
            usage(argv);
            return;
        }
    }

    *infile >> noskipws;
    
    unsigned char ch;
    for ( ;; ) {
        *infile >> ch;
    if (!*infile) break;
        utf.next(ch);
    }

    if ( infile != &cin ) {
        delete infile;		// close file, do not delete cin!
    }
    
    #endif
}