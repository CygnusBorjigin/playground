const currency1 = document.getElementById("currency-one");
const currency2 = document.getElementById("currency-two");
const quantity1 = document.getElementById("quantity-one");
const quantity2 = document.getElementById("quantity-two");
const swapButton = document.getElementById("swap");
const rate = document.getElementById("rate");

// fetch exchange rate and update DOM
const calculate = () => {
    const targetCurrency = currency1.value;
    const resultCurrency = currency2.value;

    fetch(`https://api.exchangerate-api.com/v4/latest/${targetCurrency}`)
    .then(res => res.json())
    .then(data => {
      const exchangeRate = data.rates[resultCurrency];
      rate.innerText = `1 ${targetCurrency} = ${exchangeRate} ${resultCurrency}`;

      quantity2.value = (quantity1.value * exchangeRate).toFixed(2);
    });
}

// event listeners
currency1.addEventListener("change", calculate);
currency2.addEventListener("change", calculate);
quantity1.addEventListener("input", calculate);
quantity2.addEventListener("input", calculate);
swapButton.addEventListener('click', () => {
    const temp = currency1.value;
    currency1.value = currency2.value;
    currency2.value = temp;
    calculate();
})

calculate();