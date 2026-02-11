# Harding Smalltalk Website

This directory contains the static website for Harding Smalltalk.

## Testing Code Examples

All code examples in the website are tested in `tests/test_website_examples.nim`.
The tests verify that the examples shown in the documentation work correctly.

Before committing changes to website content:
1. Run tests: `nimble test`
2. If example changed, update the corresponding test
3. Verify in REPL or with `harding script.hrd`

## Structure

This website uses a **content-driven static site** approach:

```
website/
├── content/           # Markdown content (edit these!)
│   ├── index.md      # Home page content
│   ├── why.md        # "Why Harding?" page
│   ├── features.md   # Features page
│   └── docs.md       # Documentation page
├── dist/             # Generated HTML (do not edit directly)
│   ├── index.html
│   ├── why.html
│   ├── features.html
│   ├── docs.html
│   ├── styles.css
│   └── script.js
├── styles.css        # Source styles
└── script.js         # Source scripts
```

## How to Update Content

1. **Edit the markdown files** in `content/` - these are the source of truth
2. **Ask Claude to regenerate** the HTML site
3. **The `dist/` folder** contains the generated site ready for deployment

### Example Workflow

```bash
# Edit content files
vim content/why.md

# Ask Claude: "Regenerate the website from the content files"
# Claude updates the dist/ folder with new HTML

# Preview locally:
cd website/dist
python -m http.server 8000
# Open http://localhost:8000
```

## Content Format

Content files use YAML frontmatter for metadata:

```markdown
---
title: Page Title
---

## Section Heading

Regular markdown content here.
```

## Pages

- **Home** (`index.html`) - Hero, features, quick start
- **Why Harding?** (`why.html`) - The story behind Harding Smalltalk
- **Features** (`features.html`) - Detailed feature list with code examples
- **Documentation** (`docs.html`) - Getting started guide and links

## Deployment

The `dist/` folder contains a completely static website. Deploy it to any static hosting.

To deploy:

```bash
# The dist/ folder can be deployed directly
# Copy contents to your web server, GitHub Pages, Netlify, etc.
```

## Design

- Modern dark theme with gold/copper accents
- Inter font for text, JetBrains Mono for code
- Fully responsive
- Smooth animations and transitions

### Mobile Overflow Notes

The CSS uses several techniques to prevent horizontal overflow on mobile:

- **`overflow-x: hidden`** on `html` and `body` - but this alone is unreliable on mobile browsers
- **`width: 100%`** on `.container` - prevents the container from expanding beyond its parent when grid content pushes wider
- **`overflow: hidden`** on `.hero .container` - clips content that exceeds the container
- **`min-width: 0`** on `.hero-content` and `.hero-code` - overrides the CSS Grid default `min-width: auto`, which otherwise lets grid items expand based on content size (e.g. `pre` blocks with `white-space: pre`)

When adding new sections with code blocks or grids, ensure grid children have `min-width: 0` or `overflow: hidden` to prevent content from pushing the layout wider than the viewport on mobile.

## License

MIT (same as Harding)
