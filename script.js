// SIMULATING DATA STORE (The Haskell "customers.txt")
let users = JSON.parse(localStorage.getItem('quantum_users')) || [];
let currentUser = null;
let activeOTP = null;
let attempts = 3;
let pendingWithdrawal = 0;

// TAB NAVIGATION
function switchTab(mode) {
    document.getElementById('login-form').classList.toggle('hidden', mode !== 'login');
    document.getElementById('reg-form').classList.toggle('hidden', mode !== 'reg');
    document.getElementById('btn-tab-login').classList.toggle('active', mode === 'login');
    document.getElementById('btn-tab-reg').classList.toggle('active', mode === 'reg');
}

// LOGIC: REGISTER (Haskell 'register')
function handleRegister() {
    const uid = document.getElementById('r-uid').value;
    const name = document.getElementById('r-name').value;
    const pass = document.getElementById('r-pass').value;
    const phone = document.getElementById('r-phone').value;

    if(!uid || !name || !pass) return alert("All fields are required!");
    if(users.find(u => u.uid === uid)) return alert("User ID already exists!");

    users.push({ uid, name, pass, phone, balance: 0 });
    saveData();
    alert("Identity Created! Please Login.");
    switchTab('login');
}

// LOGIC: LOGIN (Haskell 'login')
function handleLogin() {
    const uid = document.getElementById('l-uid').value;
    const pass = document.getElementById('l-pass').value;

    const user = users.find(u => u.uid === uid && u.pass === pass);
    if(user) {
        currentUser = user;
        enterDashboard();
    } else {
        alert("Access Denied: Invalid Credentials");
    }
}

function enterDashboard() {
    document.getElementById('auth-card').classList.add('hidden');
    document.getElementById('dashboard').classList.remove('hidden');
    document.getElementById('dashboard').classList.add('animate-in');
    updateUI();
}

function updateUI() {
    document.getElementById('display-name').innerText = currentUser.name;
    document.getElementById('display-balance').innerText = `$${currentUser.balance.toFixed(2)}`;
}

// LOGIC: BANKING (Haskell 'depositMoney' / 'withdrawMoney')
function deposit() {
    const amt = parseFloat(document.getElementById('tx-amount').value);
    if(isNaN(amt) || amt <= 0) return alert("Enter valid amount");
    
    currentUser.balance += amt;
    syncAndSave();
    alert("Deposit Successful");
}

function requestWithdrawal() {
    const amt = parseFloat(document.getElementById('tx-amount').value);
    if(isNaN(amt) || amt <= 0 || amt > currentUser.balance) return alert("Insufficient funds or invalid amount");
    
    const verify = prompt("Confirm Withdrawal: Enter Password");
    if(verify !== currentUser.pass) return alert("Wrong Password");

    // Haskell Logic: POSIX Time mod 1000
    activeOTP = Math.floor(Date.now() / 1000) % 1000;
    pendingWithdrawal = amt;
    attempts = 3;

    document.getElementById('otp-display').innerText = activeOTP.toString().padStart(3, '0');
    document.getElementById('otp-status').innerText = `Attempts remaining: ${attempts}`;
    document.getElementById('otp-modal').classList.remove('hidden');
}

function verifyOTP() {
    const input = document.getElementById('otp-input').value;
    if(parseInt(input) === activeOTP) {
        currentUser.balance -= pendingWithdrawal;
        syncAndSave();
        alert("Withdrawal Complete");
        closeOTP();
    } else {
        attempts--;
        if(attempts <= 0) {
            alert("Security Breach: Transaction Cancelled");
            closeOTP();
        } else {
            document.getElementById('otp-status').innerText = `Incorrect! Attempts left: ${attempts}`;
        }
    }
}

function syncAndSave() {
    const idx = users.findIndex(u => u.uid === currentUser.uid);
    users[idx] = currentUser;
    saveData();
    updateUI();
    document.getElementById('tx-amount').value = '';
}

function saveData() { localStorage.setItem('quantum_users', JSON.stringify(users)); }
function logout() { location.reload(); }
function closeOTP() { document.getElementById('otp-modal').classList.add('hidden'); document.getElementById('otp-input').value = ''; }