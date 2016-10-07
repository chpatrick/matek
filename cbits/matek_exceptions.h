#include <exception>
#include <functional>
#include <stdexcept>

struct EigenException : public std::exception {
  EigenException(const char* errorMessage) : errorMessage(errorMessage) {
  }

  virtual const char* what() const throw() {
    return errorMessage;
  }

private:
  const char* errorMessage; // Must be statically allocated.
};

typedef const char* CEigenException;

CEigenException matek_catch(const std::function<void()>& f) {
  try {
    f();

    return nullptr;
  } catch (const EigenException& ex) {
    return ex.what();
  }
}

