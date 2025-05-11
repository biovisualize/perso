// Constants
const SPLIT_DURATION = 1000; // Duration of split animation in ms
const MOVE_DURATION = 1000; // Duration of movement animation in ms
const DISAPPEAR_DURATION = 500; // Duration of disappear animation in ms
const MIN_CIRCLE_RADIUS = 50;
const MAX_CIRCLE_RADIUS = 200;
const RED_FLASH_DURATION = 300; // Shorter duration for red flash
const BLUE_FLASH_DURATION = 1000; // Longer duration for blue flash
const RED_FLASH_OPACITY = 200; // Maximum opacity for red flash
const BLUE_FLASH_OPACITY = 100; // Maximum opacity for blue flash
const MIN_BLUE_CIRCLE_SPEED = 1; // Minimum speed for blue circles
const MAX_BLUE_CIRCLE_SPEED = 4; // Maximum speed for blue circles
const MIN_BLUE_CIRCLE_SIZE_CHANGE_SPEED = 0.2; // Minimum speed of size change
const MAX_BLUE_CIRCLE_SIZE_CHANGE_SPEED = 0.8; // Maximum speed of size change
const NUM_BLUE_CIRCLES = 15; // Number of blue circles to create
const MIN_BLUE_CIRCLE_RADIUS = 20; // Minimum radius for blue circles
const MAX_BLUE_CIRCLE_RADIUS = 100; // Maximum radius for blue circles
const MAX_SCORE = 100; // Maximum score before reset
const SCORE_MULTIPLIER = 10; // Base score multiplier for multiple slices

// Circle class
class Circle {
    constructor(x, y, radius, color = [255, 0, 0]) {
        this.x = x;
        this.y = y;
        this.radius = radius;
        this.color = color;
        this.isSplitting = false;
        this.splitProgress = 0;
        this.splitStartTime = 0;
        this.splitDirection = 0;
        this.isMoving = false;
        this.moveStartTime = 0;
        this.startX = x;
        this.startY = y;
        this.targetX = x;
        this.targetY = y;
        this.isDisappearing = false;
        this.disappearStartTime = 0;
        this.opacity = 150;
        
        // Add movement properties for blue circles
        if (color[0] === 0 && color[1] === 150 && color[2] === 255) {
            this.direction = random(TWO_PI);
            this.speed = random(MIN_BLUE_CIRCLE_SPEED, MAX_BLUE_CIRCLE_SPEED);
            this.targetRadius = random(MIN_BLUE_CIRCLE_RADIUS, MAX_BLUE_CIRCLE_RADIUS);
            this.sizeChangeDirection = 1;
            this.sizeChangeSpeed = random(MIN_BLUE_CIRCLE_SIZE_CHANGE_SPEED, MAX_BLUE_CIRCLE_SIZE_CHANGE_SPEED);
        }
    }

    draw() {
        if (this.isDisappearing) {
            // Calculate fade out progress
            let progress = map(millis() - this.disappearStartTime, 0, DISAPPEAR_DURATION, 0, 1);
            progress = constrain(progress, 0, 1);
            this.opacity = 150 * (1 - progress);
            
            if (progress >= 1) {
                return; // Don't draw if fully disappeared
            }
            this.drawCircle(this.x, this.y);
        } else if (this.isSplitting) {
            // Draw split animation
            this.splitProgress = map(millis() - this.splitStartTime, 0, SPLIT_DURATION, 0, 1);
            this.splitProgress = constrain(this.splitProgress, 0, 1);

            // Calculate split positions
            let splitDistance = this.radius * 2.5 * this.splitProgress;
            let x1 = this.x - cos(this.splitDirection) * splitDistance/2;
            let y1 = this.y - sin(this.splitDirection) * splitDistance/2;
            let x2 = this.x + cos(this.splitDirection) * splitDistance/2;
            let y2 = this.y + sin(this.splitDirection) * splitDistance/2;

            // Calculate tension and control points
            let tension = 1 - this.splitProgress;
            let perpAngle = this.splitDirection + PI/2;
            
            // Draw the organic blob
            noStroke();
            fill(this.color[0], this.color[1], this.color[2], this.opacity);
            
            // Draw the main circles
            this.drawCircle(x1, y1);
            this.drawCircle(x2, y2);
            
            // Calculate the elliptical connection
            let centerX = (x1 + x2) / 2;
            let centerY = (y1 + y2) / 2;
            let connectionWidth = splitDistance + this.radius * 2;
            let connectionHeight = this.radius * 2 * tension;
            
            // Draw the elliptical connection
            push();
            translate(centerX, centerY);
            rotate(this.splitDirection);
            ellipse(0, 0, connectionWidth, connectionHeight);
            pop();

        } else if (this.isMoving) {
            // Draw movement animation
            let moveProgress = map(millis() - this.moveStartTime, 0, MOVE_DURATION, 0, 1);
            moveProgress = constrain(moveProgress, 0, 1);
            
            // Use easeOutQuad for smoother animation
            moveProgress = 1 - (1 - moveProgress) * (1 - moveProgress);
            
            this.x = lerp(this.startX, this.targetX, moveProgress);
            this.y = lerp(this.startY, this.targetY, moveProgress);
            
            this.drawCircle(this.x, this.y);
        } else {
            // Handle blue circle movement and size change
            if (this.color[0] === 0 && this.color[1] === 150 && this.color[2] === 255) {
                // Update position
                this.x += cos(this.direction) * this.speed;
                this.y += sin(this.direction) * this.speed;
                
                // Bounce off edges
                if (this.x < 0 || this.x > width) {
                    this.direction = PI - this.direction;
                }
                if (this.y < 0 || this.y > height) {
                    this.direction = -this.direction;
                }
                
                // Update size with individual speed
                this.radius += this.sizeChangeDirection * this.sizeChangeSpeed;
                
                // Change direction when reaching size limits
                if (this.radius >= MAX_BLUE_CIRCLE_RADIUS) {
                    this.radius = MAX_BLUE_CIRCLE_RADIUS;
                    this.sizeChangeDirection = -1;
                } else if (this.radius <= MIN_BLUE_CIRCLE_RADIUS) {
                    this.radius = MIN_BLUE_CIRCLE_RADIUS;
                    this.sizeChangeDirection = 1;
                }
            }
            
            this.drawCircle(this.x, this.y);
        }
    }

    drawCircle(x, y) {
        noStroke();
        fill(this.color[0], this.color[1], this.color[2], this.opacity);
        ellipse(x, y, this.radius * 2, this.radius * 2, 128); // Using ellipse with 128 segments
    }

    startSplit() {
        this.isSplitting = true;
        this.splitStartTime = millis();
        this.splitDirection = random(TWO_PI);
        // Activate screen shake
        screenShake.isActive = true;
        screenShake.startTime = millis();
    }

    startMove(targetX, targetY) {
        this.isMoving = true;
        this.moveStartTime = millis();
        this.startX = this.x;
        this.startY = this.y;
        this.targetX = targetX;
        this.targetY = targetY;
    }

    startDisappear() {
        this.isDisappearing = true;
        this.disappearStartTime = millis();
    }

    checkIntersection(x1, y1, x2, y2) {
        // Vector from point 1 to point 2
        let dx = x2 - x1;
        let dy = y2 - y1;
        
        // Vector from point 1 to circle center
        let fx = this.x - x1;
        let fy = this.y - y1;
        
        // Length of line segment squared
        let len2 = dx * dx + dy * dy;
        
        // Project circle center onto line
        let t = (fx * dx + fy * dy) / len2;
        t = constrain(t, 0, 1);
        
        // Closest point on line to circle center
        let px = x1 + t * dx;
        let py = y1 + t * dy;
        
        // Distance from closest point to circle center
        let dist2 = (this.x - px) * (this.x - px) + (this.y - py) * (this.y - py);
        
        return dist2 <= this.radius * this.radius;
    }
}

// Global variables
let circles = [];
let isDrawing = false;
let lineStartX, lineStartY;
let lineEndX, lineEndY;
let screenFlash = {
    isActive: false,
    startTime: 0,
    color: [255, 0, 0],
    opacity: 0
};
let screenShake = {
    isActive: false,
    startTime: 0,
    intensity: 10
};
let score = 0;
let consecutiveSlices = 0;
let gameFont;
let resetButton = {
    x: 0,
    y: 0,
    width: 200,
    height: 50,
    isVisible: false
};

function preload() {
    // Load the default font
    gameFont = loadFont('https://cdnjs.cloudflare.com/ajax/libs/topcoat/0.8.0/font/SourceCodePro-Bold.otf');
}

function resetGame() {
    // Clear all circles
    circles = [];
    // Reset score and consecutive slices
    score = 0;
    consecutiveSlices = 0;
    // Add initial red circle
    circles.push(new Circle(width/2, height/2, 100));
    // Add blue circles
    for (let i = 0; i < NUM_BLUE_CIRCLES; i++) {
        let radius = random(MIN_BLUE_CIRCLE_RADIUS, MAX_BLUE_CIRCLE_RADIUS);
        let pos = findValidPosition(radius);
        circles.push(new Circle(pos.x, pos.y, radius, [0, 150, 255]));
    }
}

function setup() {
    createCanvas(windowWidth, windowHeight, WEBGL);
    // Set smooth rendering
    smooth();
    // Set the font
    textFont(gameFont);
    resetGame();
}

function windowResized() {
    resizeCanvas(windowWidth, windowHeight);
}

function draw() {
    background(0);
    
    // Apply screen shake if active
    if (screenShake.isActive) {
        let shakeProgress = map(millis() - screenShake.startTime, 0, 1000, 1, 0);
        if (shakeProgress <= 0) {
            screenShake.isActive = false;
        } else {
            let shakeX = random(-screenShake.intensity, screenShake.intensity) * shakeProgress;
            let shakeY = random(-screenShake.intensity, screenShake.intensity) * shakeProgress;
            translate(shakeX, shakeY);
        }
    }
    
    // Move to center of screen for 2D drawing
    translate(-width/2, -height/2);
    
    // Draw the growing white circle background
    let maxDimension = sqrt(width * width + height * height); // Calculate hypotenuse
    let backgroundRadius = map(min(score, MAX_SCORE), 0, MAX_SCORE, 0, maxDimension/2);
    noStroke();
    fill(255);
    ellipse(width/2, height/2, backgroundRadius * 2, backgroundRadius * 2, 256); // Increased segments for smoother circle
    
    // Draw all circles
    for (let circle of circles) {
        circle.draw();
    }
    
    // Draw the slice line while dragging
    if (isDrawing) {
        stroke(255, 0, 0);
        strokeWeight(2);
        line(lineStartX, lineStartY, mouseX, mouseY);
        noStroke();
    }
    
    // Draw screen flash if active
    if (screenFlash.isActive) {
        let flashDuration = screenFlash.color[0] === 255 ? RED_FLASH_DURATION : BLUE_FLASH_DURATION;
        let progress = map(millis() - screenFlash.startTime, 0, flashDuration, 0, 1);
        progress = constrain(progress, 0, 1);
        
        // Fade in and out
        let maxOpacity = screenFlash.color[0] === 255 ? RED_FLASH_OPACITY : BLUE_FLASH_OPACITY;
        if (progress < 0.5) {
            screenFlash.opacity = map(progress, 0, 0.5, 0, maxOpacity);
        } else {
            screenFlash.opacity = map(progress, 0.5, 1, maxOpacity, 0);
        }
        
        if (progress >= 1) {
            screenFlash.isActive = false;
        }
        
        // Draw flash overlay
        noStroke();
        fill(screenFlash.color[0], screenFlash.color[1], screenFlash.color[2], screenFlash.opacity);
        rect(0, 0, width, height);
    }
    
    // Draw score (clamped to 100)
    push();
    fill(255);
    textSize(32);
    textAlign(LEFT, TOP);
    text(`Score: ${min(score, MAX_SCORE)}`, 20, 20);
    pop();
    
    // Draw reset button if score is 100
    if (score >= MAX_SCORE) {
        resetButton.isVisible = true;
        resetButton.x = width/2 - resetButton.width/2;
        resetButton.y = height/2 - resetButton.height/2;
        
        // Draw button
        push();
        fill(50);
        stroke(255);
        strokeWeight(2);
        rect(resetButton.x, resetButton.y, resetButton.width, resetButton.height, 10);
        
        // Draw text
        fill(255);
        textSize(24);
        textAlign(CENTER, CENTER);
        text("Reset Game", resetButton.x + resetButton.width/2, resetButton.y + resetButton.height/2);
        pop();
    }
    
    // Check if there are no red circles left
    let hasRedCircle = circles.some(circle => 
        circle.color[0] === 255 && circle.color[1] === 0 && circle.color[2] === 0
    );
    
    if (!hasRedCircle) {
        // Add a new red circle
        let radius = random(MIN_CIRCLE_RADIUS, MAX_CIRCLE_RADIUS);
        let x = random(radius, width - radius);
        let y = random(radius, height - radius);
        circles.push(new Circle(x, y, radius));
    }
}

function mousePressed() {
    if (resetButton.isVisible) {
        // Check if click is within reset button
        if (mouseX > resetButton.x && mouseX < resetButton.x + resetButton.width &&
            mouseY > resetButton.y && mouseY < resetButton.y + resetButton.height) {
            resetGame();
            return;
        }
    }
    
    isDrawing = true;
    lineStartX = mouseX;
    lineStartY = mouseY;
}

function mouseDragged() {
    lineEndX = mouseX;
    lineEndY = mouseY;
}

function mouseReleased() {
    if (isDrawing) {
        // Check for intersections
        let intersectedCircles = circles.filter(circle => 
            circle.checkIntersection(lineStartX, lineStartY, lineEndX, lineEndY)
        );
        
        // Check if any blue circles were intersected
        let blueCirclesIntersected = intersectedCircles.filter(circle => 
            circle.color[0] === 0 && circle.color[1] === 150 && circle.color[2] === 255
        );
        
        if (blueCirclesIntersected.length > 0) {
            // Flash screen red
            screenFlash.isActive = true;
            screenFlash.startTime = millis();
            screenFlash.color = [255, 0, 0];
            screenFlash.opacity = 0;
            // Decrement score by number of blue circles hit
            score = max(0, score - blueCirclesIntersected.length);
        } else if (intersectedCircles.length > 2) {
            // Check if all intersected circles are red
            let allRed = intersectedCircles.every(circle => 
                circle.color[0] === 255 && circle.color[1] === 0 && circle.color[2] === 0
            );
            
            if (allRed) {
                // Flash screen light blue
                screenFlash.isActive = true;
                screenFlash.startTime = millis();
                screenFlash.color = [0, 150, 255];
                screenFlash.opacity = 0;
                
                // Calculate score based on number of circles sliced
                let numRedCircles = intersectedCircles.length;
                let scoreIncrement = (numRedCircles - 2) * SCORE_MULTIPLIER;
                score += scoreIncrement;
                consecutiveSlices++;
                
                // Multiple circles disappear behavior
                for (let circle of intersectedCircles) {
                    circle.startDisappear();
                }
                
                // Remove circles after fade out
                setTimeout(() => {
                    circles = circles.filter(circle => !circle.isDisappearing);
                }, DISAPPEAR_DURATION);
            }
        } else if (intersectedCircles.length > 0) {
            // Single or double red circle split behavior
            let redCircles = intersectedCircles.filter(circle => 
                circle.color[0] === 255 && circle.color[1] === 0 && circle.color[2] === 0
            );
            
            for (let circle of redCircles) {
                circle.startSplit();
                
                setTimeout(() => {
                    // Remove the original circle
                    let index = circles.indexOf(circle);
                    if (index > -1) {
                        circles.splice(index, 1);
                    }
                    
                    // Create two new circles with random sizes
                    let newRadius1 = random(MIN_CIRCLE_RADIUS, circle.radius * 0.7);
                    let newRadius2 = random(MIN_CIRCLE_RADIUS, circle.radius * 0.7);
                    
                    // Find valid positions for new circles
                    let pos1 = findValidPosition(newRadius1);
                    let pos2 = findValidPosition(newRadius2);
                    
                    // Create new circles at split positions
                    let circle1 = new Circle(
                        circle.x - cos(circle.splitDirection) * circle.radius,
                        circle.y - sin(circle.splitDirection) * circle.radius,
                        newRadius1
                    );
                    let circle2 = new Circle(
                        circle.x + cos(circle.splitDirection) * circle.radius,
                        circle.y + sin(circle.splitDirection) * circle.radius,
                        newRadius2
                    );
                    
                    // Add new circles
                    circles.push(circle1);
                    circles.push(circle2);
                    
                    // Start movement animation
                    setTimeout(() => {
                        circle1.startMove(pos1.x, pos1.y);
                        circle2.startMove(pos2.x, pos2.y);
                    }, 50);
                }, SPLIT_DURATION);
            }
        }
    }
    isDrawing = false;
}

// Helper function to find a valid position for a new circle
function findValidPosition(radius) {
    let maxAttempts = 100;
    let attempts = 0;
    
    while (attempts < maxAttempts) {
        let x = random(radius, width - radius);
        let y = random(radius, height - radius);
        
        // Check if position is valid (no overlap with other circles)
        let isValid = true;
        for (let circle of circles) {
            let dx = x - circle.x;
            let dy = y - circle.y;
            let minDistance = radius + circle.radius;
            if (dx * dx + dy * dy < minDistance * minDistance) {
                isValid = false;
                break;
            }
        }
        
        if (isValid) {
            return {x, y};
        }
        attempts++;
    }
    
    // If no valid position found, return center
    return {x: width/2, y: height/2};
} 