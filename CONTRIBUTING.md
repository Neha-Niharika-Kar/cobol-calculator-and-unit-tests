# Contributing to COBOL Calculator

Thank you for your interest in contributing to this project! This document provides guidelines for contributing.

## 🤝 How to Contribute

### Reporting Issues

If you find a bug or have a suggestion:

1. Check if the issue already exists in the [Issues](../../issues) section
2. If not, create a new issue with:
   - Clear title and description
   - Steps to reproduce (for bugs)
   - Expected vs actual behavior
   - Your environment (OS, GnuCOBOL version)

### Submitting Changes

1. **Fork the repository**
   ```bash
   git clone https://github.com/YOUR-USERNAME/cobol-calculator.git
   cd cobol-calculator
   ```

2. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Make your changes**
   - Follow the existing code style
   - Add tests for new features
   - Update documentation as needed

4. **Test your changes**
   ```bash
   ./build.sh
   ./test-runner.sh
   ```

5. **Commit your changes**
   ```bash
   git add .
   git commit -m "Add: Brief description of your changes"
   ```

6. **Push to your fork**
   ```bash
   git push origin feature/your-feature-name
   ```

7. **Create a Pull Request**
   - Go to the original repository
   - Click "New Pull Request"
   - Select your branch
   - Describe your changes

## 📝 Coding Standards

### COBOL Code Style

- Use meaningful variable names with proper prefixes:
  - `WS-` for Working Storage variables
  - `LS-` for Linkage Section variables
- Indent consistently (4 spaces recommended)
- Add comments for complex logic
- Follow COBOL-85 standard

### Test Guidelines

- Write tests for all new features
- Ensure all existing tests pass
- Test both normal and edge cases
- Include error condition tests

### Documentation

- Update README.md for user-facing changes
- Update implementation-guide.md for technical changes
- Add inline comments for complex code
- Keep documentation clear and concise

## 🧪 Testing Requirements

All contributions must:

1. Pass all existing tests (41/41)
2. Include new tests for new features
3. Maintain or improve code coverage
4. Pass the CI/CD pipeline

Run tests locally:
```bash
./build.sh && ./test-runner.sh
```

## 📋 Pull Request Checklist

Before submitting a PR, ensure:

- [ ] Code follows project style guidelines
- [ ] All tests pass locally
- [ ] New tests added for new features
- [ ] Documentation updated
- [ ] Commit messages are clear
- [ ] No unnecessary files included
- [ ] Branch is up to date with main

## 🎯 Areas for Contribution

We welcome contributions in these areas:

### Features
- Additional arithmetic operations (power, modulo, etc.)
- Memory functions (store, recall)
- Expression parsing
- Batch calculation mode
- GUI interface

### Testing
- Additional edge cases
- Performance tests
- Integration tests
- Stress tests

### Documentation
- Tutorial videos
- Code examples
- Translation to other languages
- API documentation

### Infrastructure
- Docker support
- Additional CI/CD platforms
- Code coverage tools
- Performance profiling

## 💡 Development Setup

1. **Install GnuCOBOL**
   ```bash
   # macOS
   brew install gnu-cobol
   
   # Ubuntu/Debian
   sudo apt-get install gnucobol
   ```

2. **Clone and setup**
   ```bash
   git clone https://github.com/YOUR-USERNAME/cobol-calculator.git
   cd cobol-calculator
   chmod +x build.sh test-runner.sh
   ```

3. **Build and test**
   ```bash
   ./build.sh
   ./test-runner.sh
   ```

## 🐛 Debugging Tips

- Check compilation logs in `build/`
- Review test logs in `reports/results/`
- Use `cobc -x -g -debug` for debugging
- Add DISPLAY statements for troubleshooting

## 📞 Getting Help

- Open an issue for questions
- Check existing documentation
- Review closed issues for similar problems

## 📜 Code of Conduct

- Be respectful and inclusive
- Provide constructive feedback
- Focus on the code, not the person
- Help others learn and grow

## 🙏 Recognition

Contributors will be:
- Listed in the project README
- Credited in release notes
- Acknowledged in documentation

Thank you for contributing to COBOL Calculator! 🎉