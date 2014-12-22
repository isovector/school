// A class representing a UW course mark
class CourseMark { 
public:
    static const int LOWER_BOUND = 0;
    static const int UPPER_BOUND = 100;
    static const int INVALID_GRADE = -1;

    // An exception class for grades that are out of bounds.
    // This class is trivial and thus implemented in place
    class InvalidGradeException {
    public:
        // A constructor for the grade in question which is problematic
        InvalidGradeException(int grade) : grade_(grade) { }
        
        // The destructor. Presumably this will never be overridden, and thus doesn't need to be virtual
        ~InvalidGradeException() { };
        
        // An accessor for the invalid grade
        int grade() const { return grade_; }
        
    private:
        int grade_;
    };    


    // An implicit constructor to convert integers into CourseMarks
    CourseMark(int grade);

    // Copy constructor
    CourseMark(const CourseMark&);

    // Destructor (virtual just in case - nonvirtual destructors are bad news bears)
    virtual ~CourseMark() { };
    
    // A sanity check for the grade
    bool invalid() const { return grade_ == INVALID_GRADE; }
    
    // Assignment operator
    CourseMark& operator=(const CourseMark&);
    
    // Comparison operators
    // These, with the above int constructor allow for easy comparisons between grades and integers
    // IE: passing a course might be the expression (grade >= 60)
    
    // NOTE: invalid grade comparisons will always return false. This is by design, and is intended to replicate
    // the behavior of NaN
    bool operator==(const CourseMark&) const;
    bool operator> (const CourseMark&) const;
    bool operator< (const CourseMark&) const;
    
    // Complements of comparison operators.
    // These are implemented in terms of the above to reduce error-prone code
    bool operator!=(const CourseMark& rightside) const { return bothValid(rightside) && !operator==(rightside); }
    bool operator>=(const CourseMark& rightside) const { return bothValid(rightside) && !operator<(rightside); }
    bool operator<=(const CourseMark& rightside) const { return bothValid(rightside) && !operator>(rightside); }
    
    // An accessor for the grade represented
    int grade() const { return grade_; }
    
private:
    // Ensures that neither this object or the one it is being compared to is invalid
    bool bothValid(const CourseMark&) const;

    int grade_;
};

// We explicitly set the grade to invalid at first. That way, if we throw an exception, and recover,
// the object is still sane
CourseMark::CourseMark(int grade) : grade_(INVALID_GRADE) {
    // If our grade isn't with-in the acceptable bounds, throw an exception
    if (grade < LOWER_BOUND || grade > UPPER_BOUND)
        throw CourseMark::InvalidGradeException(grade);
        
    grade_ = grade;
}

CourseMark::CourseMark(const CourseMark& original) : grade_(original.grade_) {
}

CourseMark& CourseMark::operator=(const CourseMark& original) {
    grade_ = original.grade_;
}

bool CourseMark::operator==(const CourseMark& rightside) const {
    return bothValid(rightside) && grade_ == rightside.grade_;
}

bool CourseMark::operator>(const CourseMark& rightside) const {
    return bothValid(rightside) && grade_ > rightside.grade_;
}

bool CourseMark::operator<(const CourseMark& rightside) const {
    return bothValid(rightside) && grade_ < rightside.grade_;
}

bool CourseMark::bothValid(const CourseMark& other) const {
    return !invalid() && !other.invalid();
}


// All functions are methods because they directly act on the object. Making them
// non-member functions would be silly and serve only to pollute the namespace.

