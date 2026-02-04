# Nemo Website

This directory contains the static website for the Nemo programming language, hosted at [nemo-lang.org](https://nemo-lang.org).

## Overview

The website is built with pure HTML, CSS, and JavaScript - no build step required. It's designed to be:

- **Fast**: Static files served directly
- **Modern**: Dark theme with gradients and smooth animations
- **Responsive**: Works on all screen sizes
- **Accessible**: Semantic HTML and keyboard navigation

## Local Development

To preview the website locally:

```bash
# Using Python
python -m http.server 8080

# Using Node.js (npx)
npx serve .

# Using PHP
php -S localhost:8080
```

Then open http://localhost:8080 in your browser.

## Structure

```
website/
├── index.html    # Main page with all sections
├── styles.css    # All styling (no external CSS frameworks)
├── script.js     # Interactive features
└── README.md     # This file
```

## Deployment

The website is automatically deployed to GitHub Pages when changes are pushed to the main branch. See `.github/workflows/website.yml` for the deployment configuration.

## Custom Domain

The site is configured to use `nemo-lang.org`. To set this up:

1. In your GitHub repository, go to Settings → Pages
2. Under "Custom domain", enter `nemo-lang.org`
3. Save and wait for DNS verification
4. Configure your DNS provider with the following records:
   - A records pointing to GitHub Pages IPs:
     - 185.199.108.153
     - 185.199.109.153
     - 185.199.110.153
     - 185.199.111.153
   - Or a CNAME record pointing to `gokr.github.io`

## Features

- **Hero Section**: Animated gradient background with code preview
- **Features Grid**: 6 key Nemo features with icons
- **Code Examples**: Tabbed interface showing different language features
- **Interactive Playground**: Browser-based code editor (simulated execution)
- **Installation Guide**: Copy-paste ready commands

## License

Same as the Nemo project (MIT).
