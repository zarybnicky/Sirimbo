const snooze = ms => new Promise(resolve => setTimeout(resolve, ms));
async function loop() {
  while (true) {
    await snooze(10000);
  }
}
loop();
