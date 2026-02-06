// ============================================
// Harding Website JavaScript
// ============================================

document.addEventListener('DOMContentLoaded', () => {
    // Mobile navigation toggle
    const navToggle = document.querySelector('.nav-toggle');
    const navMenu = document.querySelector('.nav-menu');

    if (navToggle && navMenu) {
        navToggle.addEventListener('click', () => {
            navMenu.classList.toggle('active');
        });

        // Close menu when clicking a link
        navMenu.querySelectorAll('a').forEach(link => {
            link.addEventListener('click', () => {
                navMenu.classList.remove('active');
            });
        });
    }

    // ============================================
    // Example Tabs
    // ============================================
    const tabBtns = document.querySelectorAll('.tab-btn');
    const tabPanels = document.querySelectorAll('.tab-panel');

    tabBtns.forEach(btn => {
        btn.addEventListener('click', () => {
            const tabId = btn.dataset.tab;

            // Deactivate all tabs
            tabBtns.forEach(b => b.classList.remove('active'));
            tabPanels.forEach(p => p.classList.remove('active'));

            // Activate selected tab
            btn.classList.add('active');
            const panel = document.getElementById(`tab-${tabId}`);
            if (panel) {
                panel.classList.add('active');
            }
        });
    });

    // ============================================
    // Playground
    // ============================================
    const playgroundCode = document.getElementById('playground-code');
    const runBtn = document.getElementById('run-btn');
    const clearBtn = document.getElementById('clear-btn');
    const output = document.getElementById('output');

    // Example code snippets
    const examples = {
        hello: `"Hello, World!" println`,

        factorial: `"Factorial function"
factorial := [:n |
    (n <= 1) ifTrue: [^ 1].
    ^ n * (factorial value: (n - 1))
]

"Calculate factorials"
1 to: 10 do: [:i |
    (i asString , "! = " , ((factorial value: i) asString)) println
]`,

        fibonacci: `"Fibonacci sequence"
fib := [:n |
    (n <= 1) ifTrue: [^ n].
    ^ (fib value: (n - 1)) + (fib value: (n - 2))
]

"Print first 15 Fibonacci numbers"
1 to: 15 do: [:i |
    ("F(" , (i asString) , ") = " , ((fib value: i) asString)) println
]`,

        point: `"Point class example"
Point := Object derive: #(x y)

Point extend: [
    self >> moveBy: dx and: dy [
        x := x + dx
        y := y + dy
        ^ self
    ]

    self >> distanceFromOrigin [
        ^ ((x * x) + (y * y)) sqrt
    ]

    self >> toString [
        ^ "Point(" , (x asString) , ", " , (y asString) , ")"
    ]
]

"Create and use a point"
p := Point new
p x: 3 y: 4
p toString println
p distanceFromOrigin println`
    };

    // Load example buttons
    document.querySelectorAll('.toolbar-btn').forEach(btn => {
        btn.addEventListener('click', () => {
            const example = btn.dataset.example;
            if (examples[example] && playgroundCode) {
                playgroundCode.value = examples[example];
            }
        });
    });

    // Run button (simulated)
    if (runBtn && output && playgroundCode) {
        runBtn.addEventListener('click', () => {
            const code = playgroundCode.value;
            output.innerHTML = '';

            // Simulate execution delay
            output.innerHTML = '<div class="output-line">Running...</div>';

            setTimeout(() => {
                output.innerHTML = '';

                // Simple simulation based on the code content
                const lines = simulateExecution(code);
                lines.forEach(line => {
                    const div = document.createElement('div');
                    div.className = 'output-line output';
                    div.textContent = line;
                    output.appendChild(div);
                });

                if (lines.length === 0) {
                    output.innerHTML = '<div class="output-placeholder">No output (simulated)</div>';
                }
            }, 300);
        });
    }

    // Clear button
    if (clearBtn && output) {
        clearBtn.addEventListener('click', () => {
            output.innerHTML = '<div class="output-placeholder">Click "Run" to see the output</div>';
        });
    }

    // ============================================
    // Copy to Clipboard
    // ============================================
    document.querySelectorAll('.copy-btn').forEach(btn => {
        btn.addEventListener('click', async () => {
            const text = btn.dataset.clipboard;
            if (text) {
                try {
                    await navigator.clipboard.writeText(text);
                    btn.classList.add('copied');
                    setTimeout(() => {
                        btn.classList.remove('copied');
                    }, 2000);
                } catch (err) {
                    console.error('Failed to copy:', err);
                }
            }
        });
    });

    // ============================================
    // Smooth Scroll for Anchor Links
    // ============================================
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function(e) {
            e.preventDefault();
            const target = document.querySelector(this.getAttribute('href'));
            if (target) {
                const headerOffset = 80;
                const elementPosition = target.getBoundingClientRect().top;
                const offsetPosition = elementPosition + window.pageYOffset - headerOffset;

                window.scrollTo({
                    top: offsetPosition,
                    behavior: 'smooth'
                });
            }
        });
    });

    // ============================================
    // Navbar scroll effect
    // ============================================
    const navbar = document.querySelector('.navbar');
    let lastScroll = 0;

    window.addEventListener('scroll', () => {
        const currentScroll = window.pageYOffset;

        if (currentScroll > 100) {
            navbar.style.background = 'rgba(10, 10, 15, 0.95)';
        } else {
            navbar.style.background = 'rgba(10, 10, 15, 0.8)';
        }

        lastScroll = currentScroll;
    });
});

// ============================================
// Simulated Harding Execution
// ============================================
function simulateExecution(code) {
    const output = [];
    const lines = code.split('\n');

    // Very simple simulation based on common patterns
    lines.forEach(line => {
        line = line.trim();

        // Check for println
        if (line.includes('println')) {
            // Extract what's being printed
            const match = line.match(/(.+?)\s+println/);
            if (match) {
                let content = match[1];

                // Handle string concatenation with ,
                content = content.replace(/\s*,\s*/g, '');

                // Handle simple string literals
                if (content.startsWith('"') && content.endsWith('"')) {
                    output.push(content.slice(1, -1));
                }
                // Handle numbers
                else if (/^\d+$/.test(content)) {
                    output.push(content);
                }
                // Handle variable-like outputs
                else if (content === 'count' || content === 'n' || content === 'i') {
                    output.push('5');
                }
                // Handle expressions
                else if (content.includes('asString')) {
                    if (content.includes('factorial')) {
                        const n = content.match(/value:\s*(\d+)/);
                        if (n) {
                            const num = parseInt(n[1]);
                            output.push(simulateFactorial(num).toString());
                        }
                    } else if (content.includes('fib')) {
                        const n = content.match(/value:\s*(\d+)/);
                        if (n) {
                            const num = parseInt(n[1]);
                            output.push(simulateFibonacci(num).toString());
                        }
                    } else {
                        output.push('123');
                    }
                }
                // Handle toString
                else if (content.includes('toString')) {
                    output.push('Point(3, 4)');
                }
                // Handle simple expressions
                else if (content.includes('+') || content.includes('-') || content.includes('*')) {
                    output.push('42');
                }
            }
        }

        // Check for writeline
        if (line.includes('writeline:')) {
            const match = line.match(/writeline:\s*(.+)/);
            if (match) {
                let content = match[1];
                if (content.startsWith('"') && content.endsWith('"')) {
                    output.push(content.slice(1, -1));
                } else {
                    output.push('10');
                }
            }
        }

        // Check for value println pattern
        if (line.includes('value println') || line.endsWith('println')) {
            const prevLines = lines.slice(0, lines.indexOf(line));
            const counterMatch = prevLines.find(l => l.includes('Counter') && l.includes('new'));
            const initMatch = prevLines.find(l => l.includes('initialize'));
            const incrementMatch = prevLines.filter(l => l.includes('increment')).length;

            if (counterMatch && initMatch) {
                output.push(incrementMatch.toString());
            }
        }
    });

    return output;
}

function simulateFactorial(n) {
    if (n <= 1) return 1;
    return n * simulateFactorial(n - 1);
}

function simulateFibonacci(n) {
    if (n <= 1) return n;
    return simulateFibonacci(n - 1) + simulateFibonacci(n - 2);
}
