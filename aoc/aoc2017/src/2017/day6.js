const fs = require('fs');

fs.readFile('input/q6in.txt', 'utf8', (err, data) => {
    data = data.trim();
    const banks = data.split(/\s+/).map(Number);
    const states = {};
    states[banks.join('|')] = true;
    var cycles = 0;
    while(true) {
        redistribute(banks);
        cycles++;
		var hash = banks.join('|');
		console.log(banks);
        if(states[hash] || cycles === 32) {
            break;
        }
        states[hash] = true;
    }

    console.log(cycles);
});

function redistribute(banks) {
    var idx = getLargest(banks);
    var value = banks[idx];
    banks[idx] = 0;
    while(value) {
        idx = (idx + 1) % banks.length;
        banks[idx]++;
        value--;
    }
}

function getLargest(banks) {
    var largest = 0;
    var key = 0;
    for(var i = 0; i < banks.length; i++) {
        if(banks[i] > largest) {
            largest = banks[i];
            key = i;
        }
    }
    return key;
}