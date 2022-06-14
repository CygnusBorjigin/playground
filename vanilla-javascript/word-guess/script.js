const wordChoice = ["someone", "somewhere", "doing", "something"];

const box1 = document.querySelectorAll('.box1');
const box2 = document.querySelectorAll('.box2');
const box3 = document.querySelectorAll('.box3');
const box4 = document.querySelectorAll('.box4');
const box5 = document.querySelectorAll('.box5');

const  wrongLetters = document.getElementById("wrongLetters");
const  inputContainer = document.getElementById("inputContainer");
const popup = document.getElementById("popup-container");

const selectedWord = wordChoice[Math.floor(Math.random() * wordChoice.length)];

let correctLetter = [];
let wrongLetter = [];

const displayWord = () => {
    inputContainer.innerHTML = `${selectedWord
                                        .split('')
                                        .map(eachLetter => `<span>${correctLetter.includes(eachLetter) ? eachLetter : ""}</span>`)
                                        .join("")}`;
}