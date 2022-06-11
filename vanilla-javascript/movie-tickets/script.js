const container = document.querySelector('.container');
const seats = document.querySelectorAll('.row .seat:not(.occupied)');
const count = document.getElementById('count');
const total = document.getElementById('total');
const movieSelected = document.getElementById('movie');
let ticketPrice = +movieSelected.value;

seats.forEach(eachSeat => {
    const seed = Math.random();
    if (seed < 0.2) {
        eachSeat.classList.toggle('occupied');
    }
});

const updateSelectedCount = () => {
    var number = 0;
    seats.forEach(eachSeat => {
        if (eachSeat.classList.contains('selected')) {
            number += 1;
        }
    });
    count.innerHTML = number;

    total.innerHTML = number * ticketPrice;
};

movieSelected.addEventListener('change', event => {
    ticketPrice = event.target.value;
    updateSelectedCount();
})

container.addEventListener('click', (event) => {
  if (event.target.classList.contains('seat') && !event.target.classList.contains('occupied')) {
    event.target.classList.toggle('selected');
    updateSelectedCount();
  }
})