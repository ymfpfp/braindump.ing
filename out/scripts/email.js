"use strict";
window.onload = () => {
    const form = document.querySelector("form");
    const input = document.querySelector("form input");
    const submitButton = document.querySelector("form button");
    const reset = () => {
        submitButton.disabled = false;
        submitButton.innerText = "Subscribe";
    };
    input.addEventListener("focus", reset);
    input.addEventListener("input", reset);
    const invalid = (text) => {
        submitButton.innerText = text;
        submitButton.disabled = true;
        form.style.animation = "none";
        form.offsetHeight; // Force reflow so the animation replays on every invalid attempt.
        form.style.animation = "shake 0.4s";
    };
    form.addEventListener("submit", async (event) => {
        console.log(event);
        event.preventDefault();
        const data = new FormData(form);
        const email = data.get("email");
        if (!email)
            return;
        input.value = "";
        submitButton.innerText = "Subscribing...";
        try {
            const response = await fetch("https://api.braindump.ing/subscribe", {
                method: "POST",
                body: JSON.stringify({ email }),
            });
            if (response.ok) {
                submitButton.disabled = true;
                submitButton.innerText = "Subscribed!";
                return;
            }
            throw new Error();
        }
        catch {
            invalid("Invalid email!");
        }
    });
};
