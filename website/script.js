// Matrix-style Code Rain Background with Particles
class CodeRain {
    constructor() {
        this.canvas = document.getElementById('matrix-bg');
        this.ctx = this.canvas.getContext('2d');
        this.resize();
        
        // Code snippets to display
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
        
        // Floating particles
        this.particles = [];
        this.createParticles();
        
        window.addEventListener('resize', () => this.resize());
    }
    
    resize() {
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
            
            // Move particle
            particle.x += particle.speedX;
            particle.y += particle.speedY;
            
            // Wrap around edges
            if (particle.x < 0) particle.x = this.canvas.width;
            if (particle.x > this.canvas.width) particle.x = 0;
            if (particle.y < 0) particle.y = this.canvas.height;
            if (particle.y > this.canvas.height) particle.y = 0;
        });
    }
    
    draw() {
        // Semi-transparent black to create trail effect
        this.ctx.fillStyle = 'rgba(15, 23, 42, 0.05)';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
        
        // Draw floating particles
        this.drawParticles();
        
        // Set text properties for code rain
        this.ctx.font = '14px "Fira Code", monospace';
        
        // Draw code rain
        for (let i = 0; i < this.drops.length; i++) {
            // Random code snippet
            const text = this.codeSnippets[Math.floor(Math.random() * this.codeSnippets.length)];
            
            // Gradient color for leading character
            const yPos = this.drops[i] * 20;
            const brightness = Math.max(0.3, 1 - (yPos % 100) / 100);
            this.ctx.fillStyle = `rgba(99, 102, 241, ${brightness})`;
            
            // Draw the text
            this.ctx.fillText(text, i * 20, yPos);
            
            // Reset drop to top randomly after it crosses screen
            if (yPos > this.canvas.height && Math.random() > 0.975) {
                this.drops[i] = 0;
            }
            
            // Increment Y coordinate
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

// Initialize code rain animation
const codeRain = new CodeRain();
codeRain.start();

// Smooth scroll for navigation links
document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function (e) {
        e.preventDefault();
        const target = document.querySelector(this.getAttribute('href'));
        if (target) {
            target.scrollIntoView({
                behavior: 'smooth',
                block: 'start'
            });
        }
    });
});

// Active nav link on scroll
const sections = document.querySelectorAll('section[id]');
const navLinks = document.querySelectorAll('.nav-link');

window.addEventListener('scroll', () => {
    let current = '';
    sections.forEach(section => {
        const sectionTop = section.offsetTop;
        const sectionHeight = section.clientHeight;
        if (pageYOffset >= sectionTop - 200) {
            current = section.getAttribute('id');
        }
    });

    navLinks.forEach(link => {
        link.classList.remove('active');
        if (link.getAttribute('href') === `#${current}`) {
            link.classList.add('active');
        }
    });
});

// Playground code execution (Real GLaDOS execution!)
const BACKEND_URL = 'http://localhost:5000';

async function runCode() {
    const code = document.getElementById('code-editor').value;
    const output = document.getElementById('output');
    
    if (!code.trim()) {
        output.textContent = 'Please enter some code first!';
        output.style.color = 'var(--warning)';
        return;
    }
    
    // Show loading
    output.textContent = '⏳ Executing...';
    output.style.color = 'var(--warning)';
    
    try {
        const response = await fetch(`${BACKEND_URL}/api/execute`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ code })
        });
        
        const data = await response.json();
        
        if (data.success) {
            output.textContent = data.output;
            output.style.color = 'var(--success)';
        } else {
            output.textContent = data.error;
            output.style.color = 'var(--error)';
        }
        
    } catch (error) {
        // Backend not available - show helpful message
        output.textContent = `❌ Backend not available\n\n` +
            `To enable the playground:\n` +
            `1. Install dependencies: pip3 install -r requirements.txt\n` +
            `2. Build GLaDOS: make\n` +
            `3. Start backend: python3 backend.py\n` +
            `4. Refresh this page\n\n` +
            `Error: ${error.message}`;
        output.style.color = 'var(--error)';
    }
}

// Check backend health on page load
async function checkBackendHealth() {
    try {
        const response = await fetch(`${BACKEND_URL}/api/health`);
        const data = await response.json();
        
        if (data.status === 'ok') {
            console.log('✅ GLaDOS backend is ready!');
        } else {
            console.warn('⚠️ GLaDOS executable not found. Run "make" first.');
        }
    } catch (error) {
        console.warn('⚠️ Backend not running. Start with: python3 backend.py');
    }
}

// Check backend on load
checkBackendHealth();

// Add syntax highlighting to code editor (basic)
const codeEditor = document.getElementById('code-editor');
if (codeEditor) {
    codeEditor.addEventListener('input', function() {
        // You could add real-time syntax highlighting here
        // For now, we keep it simple
    });
}

// Keyboard shortcut for running code (Ctrl/Cmd + Enter)
if (codeEditor) {
    codeEditor.addEventListener('keydown', function(e) {
        if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
            e.preventDefault();
            runCode();
        }
    });
}

// Add animation on scroll
const observerOptions = {
    threshold: 0.1,
    rootMargin: '0px 0px -100px 0px'
};

const observer = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
        if (entry.isIntersecting) {
            entry.target.style.opacity = '1';
            entry.target.style.transform = 'translateY(0)';
        }
    });
}, observerOptions);

// Observe all feature cards, step cards, and doc cards
document.querySelectorAll('.feature-card, .step-card, .doc-card').forEach(card => {
    card.style.opacity = '0';
    card.style.transform = 'translateY(20px)';
    card.style.transition = 'all 0.6s ease-out';
    observer.observe(card);
});

// Easter egg: Konami code
let konamiCode = [];
const konamiSequence = ['ArrowUp', 'ArrowUp', 'ArrowDown', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'ArrowLeft', 'ArrowRight', 'b', 'a'];

document.addEventListener('keydown', (e) => {
    konamiCode.push(e.key);
    konamiCode = konamiCode.slice(-10);
    
    if (konamiCode.join(',') === konamiSequence.join(',')) {
        document.body.style.animation = 'rainbow 2s linear infinite';
        setTimeout(() => {
            document.body.style.animation = '';
        }, 5000);
    }
});

// Add rainbow animation for easter egg
const style = document.createElement('style');
style.textContent = `
    @keyframes rainbow {
        0% { filter: hue-rotate(0deg); }
        100% { filter: hue-rotate(360deg); }
    }
`;
document.head.appendChild(style);

console.log('%c🧪 GLaDOS - Generic Language and Data Operand Syntax', 'font-size: 20px; color: #6366f1; font-weight: bold;');
console.log('%cWelcome to the GLaDOS developer console!', 'font-size: 14px; color: #94a3b8;');
console.log('%cTry the Konami code for a surprise... ⬆️⬆️⬇️⬇️⬅️➡️⬅️➡️BA', 'font-size: 12px; color: #8b5cf6;');
