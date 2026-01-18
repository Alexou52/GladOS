# GLaDOS Website

This directory contains the official documentation website for GLaDOS.

## 🌐 Structure

```
website/
├── index.html          # Landing page
├── docs.html           # Documentation viewer
├── style.css           # Main styles
├── docs-style.css      # Documentation-specific styles
├── script.js           # Homepage interactive features
└── docs.js             # Documentation page logic

documentation/
├── introduction.md     # Getting started guide
├── syntax.md          # Language syntax reference
├── builtins.md        # Built-in functions reference
└── examples.md        # Code examples and patterns
```

## 🚀 Features

- **Modern Design**: Dark theme with gradient accents
- **Interactive Playground**: Try GLaDOS code directly in the browser
- **Markdown Documentation**: Easy-to-update documentation files
- **Responsive Layout**: Works on desktop, tablet, and mobile
- **Syntax Highlighting**: Color-coded code examples
- **Table of Contents**: Auto-generated navigation for docs
- **Copy to Clipboard**: One-click code copying

## 🛠️ Local Development

### Option 1: Python Simple Server

```bash
cd website
python3 -m http.server 8000
```

Then open http://localhost:8000

### Option 2: Node.js http-server

```bash
npm install -g http-server
cd website
http-server -p 8000
```

### Option 3: PHP Built-in Server

```bash
cd website
php -S localhost:8000
```

## 📝 Adding Documentation

1. Create a new `.md` file in the `documentation/` directory
2. Write your content in Markdown format
3. Add a link to `docs.html` in the sidebar navigation
4. The page will automatically render when loaded

Example markdown file:

```markdown
# My New Feature

## Overview

Description of the feature...

## Example

\```scheme
(define (my-function x)
  (+ x 1))
\```

## See Also

- [Related Topic](other-page.md)
```

## 🎨 Customization

### Colors

Edit CSS variables in `style.css`:

```css
:root {
    --primary: #6366f1;
    --secondary: #8b5cf6;
    --background: #0f172a;
    /* ... */
}
```

### Features

Modify sections in `index.html`:
- Hero section
- Features grid
- Getting Started steps
- Playground

## 📦 Dependencies

- **marked.js**: Markdown parser (loaded from CDN)
- **Google Fonts**: Inter & Fira Code fonts

## 🚀 Deployment

### GitHub Pages

1. Push to GitHub
2. Go to Settings → Pages
3. Select branch `main` and folder `/website`
4. Your site will be live at `https://username.github.io/glados`

### Netlify

1. Connect your repository
2. Set build directory to `website`
3. Deploy!

### Vercel

```bash
cd website
vercel
```

## 🔧 Browser Support

- Chrome/Edge: ✅
- Firefox: ✅
- Safari: ✅
- Mobile browsers: ✅

## 📄 License

Same as GLaDOS project.

---

Built with ❤️ for the GLaDOS community
