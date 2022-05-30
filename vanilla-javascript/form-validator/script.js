const form = document.getElementById('form');
const username = document.getElementById('username');
const email = document.getElementById('email');
const password = document.getElementById('password');
const password2 = document.getElementById('password2');

const showError = (input, message) => {
    const formControl = input.parentElement;
    formControl.className='form-control error';
    const small = formControl.querySelector('small');
    small.innerText = message;
};

const showSuccess = (input) => {
    const formControl = input.parentElement;
    formControl.className = 'form-control success';
}

form.addEventListener('submit', event => {
    event.preventDefault();

    if (username.value === '') showError(username, 'A username is required');
    if (email.value === '' || email.value.includes('@') === false) showError(email, 'A email is required');
    if (password.value.length < 6) showError(password, 'Password need to have more than 6 characters');
    if (password.value !== password2.value) showError(password2, 'the two passwords does not match');
});