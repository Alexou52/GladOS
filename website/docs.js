// Matrix-style Code Rain Background with Particles (same as index)
class CodeRain {
    constructor() {
        this.canvas = document.getElementById('matrix-bg');
        if (!this.canvas) return;
        
        this.ctx = this.canvas.getContext('2d');
        this.resize();
        
        this.codeSnippets = [
            '(define ',
            'lambda ',
            'if ',
            'eq? ',
            '#t ',
            '#f ',
            '+ ',
            '- ',
            '* ',
            'div ',
            'mod ',
            '< ',
            '( ',
            ') ',
            'fact ',
            'fib ',
            '=> ',
            '42 ',
            '10 ',
            'x ',
            'y ',
        ];
        
        this.columns = Math.floor(this.canvas.width / 20);
        this.drops = Array(this.columns).fill(1);
        
        this.particles = [];
        this.createParticles();
        
        window.addEventListener('resize', () => this.resize());
    }
    
    resize() {
        if (!this.canvas) return;
        this.canvas.width = window.innerWidth;
        this.canvas.height = window.innerHeight;
        this.columns = Math.floor(this.canvas.width / 20);
        this.drops = Array(this.columns).fill(1);
        this.particles = [];
        this.createParticles();
    }
    
    createParticles() {
        const particleCount = Math.floor(this.canvas.width / 30);
        for (let i = 0; i < particleCount; i++) {
            this.particles.push({
                x: Math.random() * this.canvas.width,
                y: Math.random() * this.canvas.height,
                size: Math.random() * 3 + 1,
                speedX: (Math.random() - 0.5) * 0.5,
                speedY: (Math.random() - 0.5) * 0.5,
                opacity: Math.random() * 0.5 + 0.2
            });
        }
    }
    
    drawParticles() {
        this.particles.forEach(particle => {
            this.ctx.fillStyle = `rgba(99, 102, 241, ${particle.opacity})`;
            this.ctx.beginPath();
            this.ctx.arc(particle.x, particle.y, particle.size, 0, Math.PI * 2);
            this.ctx.fill();
            
            particle.x += particle.speedX;
            particle.y += particle.speedY;
            
            if (particle.x < 0) particle.x = this.canvas.width;
            if (particle.x > this.canvas.width) particle.x = 0;
            if (particle.y < 0) particle.y = this.canvas.height;
            if (particle.y > this.canvas.height) particle.y = 0;
        });
    }
    
    draw() {
        if (!this.canvas || !this.ctx) return;
        
        this.ctx.fillStyle = 'rgba(15, 23, 42, 0.05)';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
        
        this.drawParticles();
        
        this.ctx.font = '14px "Fira Code", monospace';
        
        for (let i = 0; i < this.drops.length; i++) {
            const text = this.codeSnippets[Math.floor(Math.random() * this.codeSnippets.length)];
            
            const yPos = this.drops[i] * 20;
            const brightness = Math.max(0.3, 1 - (yPos % 100) / 100);
            this.ctx.fillStyle = `rgba(99, 102, 241, ${brightness})`;
            
            this.ctx.fillText(text, i * 20, yPos);
            
            if (yPos > this.canvas.height && Math.random() > 0.975) {
                this.drops[i] = 0;
            }
            
            this.drops[i]++;
        }
    }
    
    start() {
        const animate = () => {
            this.draw();
            requestAnimationFrame(animate);
        };
        animate();
    }
}

// Initialize background animation
if (document.getElementById('matrix-bg')) {
    const codeRain = new CodeRain();
    codeRain.start();
}

// Get page parameter from URL
function getPageParam() {
    const urlParams = new URLSearchParams(window.location.search);
    return urlParams.get('page') || 'introduction';
}

// Load markdown file and render it
async function loadMarkdown(page) {
    const content = document.getElementById('docs-content');
    
    try {
        // Show loading
        content.innerHTML = '<div class="loading"><div class="spinner"></div><p>Loading documentation...</p></div>';
        
        // Fetch markdown file
        const response = await fetch(`../documentation/${page}.md`);
        if (!response.ok) {
            throw new Error('Documentation not found');
        }
        
        const markdown = await response.text();
        
        // Convert markdown to HTML
        const html = marked.parse(markdown);
        
        // Render HTML
        content.innerHTML = html;
        
        // Apply syntax highlighting
        highlightCode();
        
        // Generate table of contents
        generateTOC();
        
        // Scroll to top
        window.scrollTo({ top: 0, behavior: 'smooth' });
        
    } catch (error) {
        content.innerHTML = `
            <div class="error-state">
                <h2>😕 Documentation Not Found</h2>
                <p>The requested documentation page could not be loaded.</p>
                <p><a href="?page=introduction">Return to Introduction</a></p>
            </div>
        `;
    }
}

// Generate table of contents from headings
function generateTOC() {
    const content = document.getElementById('docs-content');
    const toc = document.getElementById('table-of-contents');
    const headings = content.querySelectorAll('h2, h3');
    
    if (headings.length === 0) {
        toc.innerHTML = '<p style="color: var(--text-muted); font-size: 0.875rem;">No sections</p>';
        return;
    }
    
    let tocHTML = '<ul>';
    let currentLevel = 2;
    
    headings.forEach((heading, index) => {
        const level = parseInt(heading.tagName[1]);
        const text = heading.textContent;
        const id = `heading-${index}`;
        
        // Add ID to heading for linking
        heading.id = id;
        
        // Handle nesting
        if (level > currentLevel) {
            tocHTML += '<ul>';
        } else if (level < currentLevel) {
            tocHTML += '</ul>';
        }
        
        tocHTML += `<li><a href="#${id}">${text}</a></li>`;
        currentLevel = level;
    });
    
    tocHTML += '</ul>';
    toc.innerHTML = tocHTML;
    
    // Add scroll spy
    setupScrollSpy();
}

// Highlight active TOC item on scroll
function setupScrollSpy() {
    const headings = document.querySelectorAll('#docs-content h2[id], #docs-content h3[id]');
    const tocLinks = document.querySelectorAll('#table-of-contents a');
    
    if (headings.length === 0) return;
    
    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                const id = entry.target.id;
                tocLinks.forEach(link => {
                    link.classList.remove('active');
                    if (link.getAttribute('href') === `#${id}`) {
                        link.classList.add('active');
                    }
                });
            }
        });
    }, {
        rootMargin: '-100px 0px -66%',
        threshold: 0
    });
    
    headings.forEach(heading => observer.observe(heading));
}

// Basic syntax highlighting for GLaDOS code
function highlightCode() {
    const codeBlocks = document.querySelectorAll('pre code');
    
    codeBlocks.forEach(block => {
        let html = block.textContent;
        
        // Comments
        html = html.replace(/;.*/g, match => `<span class="comment">${match}</span>`);
        
        // Keywords (Scheme + new syntax)
        html = html.replace(/\b(define|lambda|if|cond|let|begin|def|fn|for|while|in|range|else|elif|return|break|continue|true|false|print)\b/g, '<span class="keyword">$1</span>');
        
        // Numbers
        html = html.replace(/\b\d+\b/g, '<span class="number">$&</span>');
        
        // Booleans
        html = html.replace(/#[tf]\b/g, '<span class="keyword">$&</span>');
        
        // Built-in functions
        html = html.replace(/\b(eq\?|neq\?|<|>|<=|>=|\+|-|\*|div|mod|len|get|append|set|concat)\b/g, '<span class="function">$1</span>');
        
        // Strings
        html = html.replace(/"([^"]*)"/g, '<span class="string">"$1"</span>');
        
        block.innerHTML = html;
    });
}

// Update active nav link
function updateActiveNav() {
    const page = getPageParam();
    const navLinks = document.querySelectorAll('.docs-nav-link');
    
    navLinks.forEach(link => {
        link.classList.remove('active');
        if (link.dataset.page === page) {
            link.classList.add('active');
        }
    });
}

// Handle navigation
document.querySelectorAll('.docs-nav-link').forEach(link => {
    link.addEventListener('click', (e) => {
        e.preventDefault();
        const page = link.dataset.page;
        window.history.pushState({}, '', `?page=${page}`);
        loadMarkdown(page);
        updateActiveNav();
    });
});

// Handle browser back/forward
window.addEventListener('popstate', () => {
    loadMarkdown(getPageParam());
    updateActiveNav();
});

// Configure marked options
marked.setOptions({
    breaks: true,
    gfm: true,
    headerIds: true,
    mangle: false
});

// Load initial page
document.addEventListener('DOMContentLoaded', () => {
    loadMarkdown(getPageParam());
    updateActiveNav();
});

// Search functionality (optional enhancement)
function initSearch() {
    const searchInput = document.getElementById('search-input');
    if (!searchInput) return;
    
    searchInput.addEventListener('input', (e) => {
        const query = e.target.value.toLowerCase();
        const navLinks = document.querySelectorAll('.docs-nav-link');
        
        navLinks.forEach(link => {
            const text = link.textContent.toLowerCase();
            if (text.includes(query)) {
                link.style.display = 'block';
            } else {
                link.style.display = 'none';
            }
        });
    });
}

// Copy code to clipboard
document.addEventListener('click', (e) => {
    if (e.target.closest('pre')) {
        const pre = e.target.closest('pre');
        const code = pre.querySelector('code');
        
        if (code) {
            navigator.clipboard.writeText(code.textContent).then(() => {
                // Show feedback
                const feedback = document.createElement('div');
                feedback.textContent = 'Copied!';
                feedback.style.cssText = `
                    position: fixed;
                    top: 50%;
                    left: 50%;
                    transform: translate(-50%, -50%);
                    background: var(--success);
                    color: white;
                    padding: 1rem 2rem;
                    border-radius: 0.5rem;
                    z-index: 10000;
                    animation: fadeOut 2s forwards;
                `;
                document.body.appendChild(feedback);
                
                setTimeout(() => feedback.remove(), 2000);
            });
        }
    }
});

// Add fade out animation
const style = document.createElement('style');
style.textContent = `
    @keyframes fadeOut {
        0%, 70% {
            opacity: 1;
        }
        100% {
            opacity: 0;
        }
    }
`;
document.head.appendChild(style);

console.log('%c📖 GLaDOS Documentation', 'font-size: 16px; color: #6366f1; font-weight: bold;');
console.log('%cClick any code block to copy it to clipboard!', 'font-size: 12px; color: #94a3b8;');
