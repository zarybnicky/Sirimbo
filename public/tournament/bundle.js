define("common", ["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
});
define("admin", ["require", "exports", "react"], function (require, exports, React) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TournamentAdmin extends React.Component {
        constructor() {
            super(...arguments);
            this.state = {
                tournament: {
                    winnersRoot: 0, losersRoot: 0,
                    tournamentPlayers: {},
                    dashboardFocus: null,
                    userFocus: null,
                    nodes: {},
                },
            };
            this.socket = null;
            this.timeout = undefined;
            this.connect = () => {
                clearTimeout(this.timeout);
                this.timeout = undefined;
                this.socket = new WebSocket('wss://api.tkolymp.cz/tournament/admin/ws');
                this.socket.onmessage = e => {
                    const msg = JSON.parse(e.data);
                    if (msg['tag'] === 'StateMsg') {
                        this.setState({ tournament: msg.contents });
                    }
                };
                this.socket.onclose = () => {
                    this.timeout = this.timeout || setTimeout(this.connect, 500);
                };
                this.socket.onerror = () => {
                    this.timeout = this.timeout || setTimeout(this.connect, 500);
                };
                return this.socket;
            };
            this.open = (k) => {
                this.socket.send(JSON.stringify({
                    tag: 'OpenVoting', contents: parseInt(k, 10),
                }));
            };
            this.close = (k) => {
                this.socket.send(JSON.stringify({
                    tag: 'CloseVoting', contents: parseInt(k, 10),
                }));
            };
            this.focus = (k) => {
                this.socket.send(JSON.stringify({
                    tag: 'UserFocusNode', contents: parseInt(k, 10),
                }));
            };
            this.reset = () => {
                this.socket.send(JSON.stringify({ tag: 'ResetState' }));
            };
            this.renderBattle = (t, i, k) => {
                const n = t.nodes[k];
                if (!n)
                    return null;
                const open = () => this.open(k);
                const close = () => this.close(k);
                const focus = () => this.focus(k);
                return React.createElement("div", { key: k, className: "row mb-2" },
                    React.createElement("div", { className: "col-1 d-flex align-items-center justify-content-end" },
                        "#",
                        i),
                    React.createElement("div", { className: "col-7" },
                        React.createElement(BattleComponent, { socket: this.socket, tournament: t, battle: n })),
                    React.createElement("div", { className: "col-4" },
                        n.tag === 'DuelWaitingNode'
                            ? React.createElement("button", { onClick: open, className: "btn btn-sm btn-primary" }, "Spustit hlasov\u00E1n\u00ED") : null,
                        n.tag === 'DuelFinishedNode' && n.contents[1].victor === null
                            ? React.createElement("button", { onClick: close, className: "btn btn-sm btn-primary" }, "Ukon\u010Dit hlasov\u00E1n\u00ED") : null,
                        (t.userFocus || 0).toString() === k.toString()
                            ? 'Aktuální'
                            : React.createElement("button", { onClick: focus, className: "btn btn-sm btn-primary" }, "Nastavit jako aktu\u00E1ln\u00ED")));
            };
            this.renderPlayer = (t, k) => {
                const p = t.tournamentPlayers[k];
                return React.createElement(PlayerComponent, { key: k, socket: this.socket, playerId: k, player: p });
            };
        }
        componentDidMount() {
            this.socket = this.connect();
        }
        render() {
            const t = this.state.tournament;
            return React.createElement("main", { className: 'container' },
                React.createElement("button", { className: "btn btn-danger", onClick: this.reset }, "Restartovat turnaj"),
                React.createElement("h3", null, "Duely"),
                [2, 5, 8, 1].map((k, i) => this.renderBattle(t, i, k.toString())),
                React.createElement("h3", null, "P\u00E1ry"),
                Object.keys(t.tournamentPlayers).map(k => this.renderPlayer(t, k)));
        }
    }
    exports.TournamentAdmin = TournamentAdmin;
    class BattleComponent extends React.Component {
        constructor(props) {
            super(props);
        }
        render() {
            const n = this.props.battle;
            const t = this.props.tournament;
            if (n.tag === 'DuelWaitingNode') {
                const l = n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]] : null;
                const r = n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]] : null;
                return React.createElement("ul", { className: "list-group" },
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" }, l ? l.longName : '?'),
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" }, r ? r.longName : '?'));
            }
            else if (n.tag === 'DuelFinishedNode') {
                const res = n.contents[1];
                const l = t.tournamentPlayers[res.leftPlayer] || {};
                const r = t.tournamentPlayers[res.rightPlayer] || {};
                return React.createElement("ul", { className: "list-group" },
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" },
                        React.createElement("span", { className: res.victor === res.leftPlayer ? 'font-weight-bold' : undefined }, l.longName),
                        React.createElement("span", { className: "badge badge-primary badge-pill" }, res.leftScore)),
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" },
                        React.createElement("span", { className: res.victor === res.rightPlayer ? 'font-weight-bold' : undefined }, r.longName),
                        React.createElement("span", { className: "badge badge-primary badge-pill" }, res.rightScore)));
            }
        }
    }
    exports.BattleComponent = BattleComponent;
    class PlayerComponent extends React.Component {
        constructor(props) {
            super(props);
            this.longChange = (event) => {
                const player = Object.assign(Object.assign({}, this.state.player), { longName: event.target.value });
                this.setState({ player });
                this.props.socket.send(JSON.stringify({
                    tag: 'UpdatePlayer',
                    contents: [parseInt(this.props.playerId), player],
                }));
            };
            this.shortChange = (event) => {
                const player = Object.assign(Object.assign({}, this.state.player), { shortName: event.target.value });
                this.setState({ player });
                this.props.socket.send(JSON.stringify({
                    tag: 'UpdatePlayer',
                    contents: [parseInt(this.props.playerId), player],
                }));
            };
            this.state = { player: props.player };
        }
        componentWillReceiveProps(nextProps) {
            if (nextProps.player !== this.props.player) {
                this.setState({ player: nextProps.player });
            }
        }
        render() {
            return React.createElement("div", { className: "form-row" },
                React.createElement("div", { className: "col" },
                    React.createElement("input", { type: "text", className: "form-control", placeholder: "Dlouh\u00E9 jm\u00E9no", value: this.state.player.longName, onChange: this.longChange })),
                React.createElement("div", { className: "col" },
                    React.createElement("input", { type: "text", className: "form-control", placeholder: "Kr\u00E1tk\u00E9 jm\u00E9no", value: this.state.player.shortName, onChange: this.shortChange })));
        }
    }
    exports.PlayerComponent = PlayerComponent;
});
define("dashboard", ["require", "exports", "react"], function (require, exports, React) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var MessageType;
    (function (MessageType) {
        MessageType["Request"] = "Request";
        MessageType["ResponseSuccess"] = "ResponseSuccess";
        MessageType["ResponseStatus"] = "ResponseStatus";
        MessageType["ResponseError"] = "ResponseError";
        MessageType["ResponseUnknown"] = "ResponseUnknown";
    })(MessageType || (MessageType = {}));
    var DeviceStatus;
    (function (DeviceStatus) {
        DeviceStatus["Disconnected"] = "Disconnected";
        DeviceStatus["Available"] = "Available";
        DeviceStatus["Busy"] = "Busy";
    })(DeviceStatus || (DeviceStatus = {}));
    const base = location.origin;
    class TournamentDashboard extends React.Component {
        constructor() {
            super(...arguments);
            this.state = {
                request: getRandomRequest(),
                paymentStatus: { busy: false },
                devices: [], types: [],
                selectedDevice: undefined,
            };
            this.editStatus = (x) => {
                this.setState({ paymentStatus: Object.assign({}, this.state.paymentStatus, x) });
            };
            this.fetchDevices = () => {
                fetch(base + '/devices/connected').then(parseJSON).then((res) => {
                    if (!res.ok) {
                        throw res;
                    }
                    this.setState({
                        devices: res.json.filter((x) => x.status !== 'Disconnected'),
                    });
                });
            };
            this.fetchTypes = () => {
                fetch(base + '/types').then(parseJSON).then((res) => {
                    if (!res.ok) {
                        throw res;
                    }
                    this.setState({ types: res.json });
                });
            };
            this.onSubmit = (req) => {
                if (this.state.selectedDevice === undefined) {
                    return;
                }
                this.editStatus({ busy: true });
                let timerId;
                fetch(base + '/pay', {
                    method: 'post',
                    headers: new Headers({ 'Content-Type': 'application/json' }),
                    body: JSON.stringify({
                        amount: req.amount,
                        orderId: req.orderId,
                        paymentInfo: {
                            deviceId: this.state.selectedDevice,
                            saleReference: req.orderId,
                        },
                    }),
                }).then(parseJSON).then((res) => {
                    if (!res.ok) {
                        throw res;
                    }
                    this.editStatus({ data: Object.assign(Object.assign({}, res.json), { paymentId: res.json.paymentInfo.paymentId }) });
                    timerId = setInterval(() => this.onRefresh(res.json.paymentInfo.paymentId, timerId), 1000);
                }).catch((error) => {
                    console.log(error);
                    this.editStatus({ error, busy: false });
                    timerId && clearInterval(timerId);
                });
            };
            this.onRefresh = (id, timerId) => {
                fetch(base + `/payments/${id}`).then(parseJSON).then((res) => {
                    if (!res.ok) {
                        throw res;
                    }
                    const data = res.json;
                    if (data.status !== 'pending') {
                        this.editStatus({ data, busy: false });
                        timerId && clearInterval(timerId);
                        this.setState({ request: getRandomRequest() });
                    }
                    else {
                        this.editStatus({ data });
                    }
                }).catch((error) => {
                    console.log(error);
                    this.editStatus({ error, busy: false });
                    timerId && clearInterval(timerId);
                });
            };
            this.onDevice = (x) => this.setState({ selectedDevice: x });
        }
        componentWillMount() {
            setInterval(this.fetchDevices, 1500);
            setInterval(this.fetchTypes, 1500);
            this.fetchDevices();
            this.fetchTypes();
        }
        render() {
            return React.createElement('main', { className: 'app' }, React.createElement('header', {}, React.createElement('div', {}, this.state.types.find(x => x === 'Yomani') &&
                React.createElement('a', { href: '/yomani.html', target: '_blank' }, 'Mock Yomani'), ' ', this.state.types.find(x => x === 'Kiosk') &&
                React.createElement('a', { href: '/kiosk.html', target: '_blank' }, 'Mock Kiosk')), React.createElement('div', { style: { fontWeight: 'bold' } }, `New sale, sale no. ${this.state.request.orderId}`)), React.createElement('div', { className: 'app-inner' }, React.createElement(DeviceSelector, {
                devices: this.state.devices,
                selected: this.state.selectedDevice,
                onDevice: this.onDevice,
            }), React.createElement(Input, {
                onSubmit: this.onSubmit,
                init: this.state.request,
                disabled: this.state.paymentStatus.busy,
            }), React.createElement(PaymentView, this.state.paymentStatus)));
        }
    }
    exports.TournamentDashboard = TournamentDashboard;
    class DeviceSelector extends React.Component {
        render() {
            return React.createElement('ul', { className: 'device-selector' }, this.props.devices.map(x => React.createElement('li', {
                key: x.id,
                className: x.id === this.props.selected ? 'selected' : null,
                onClick: () => this.props.onDevice(x.id),
            }, x.name)));
        }
    }
    class PaymentView extends React.Component {
        render() {
            let inner;
            if (this.props.error) {
                inner = `Error: ${JSON.stringify(this.props.error)}`;
            }
            else if (this.props.data) {
                const { paymentId, deviceId, amount, status, messages } = this.props.data;
                inner = React.createElement('div', {}, React.createElement('div', {}, `Payment: ${paymentId}`), React.createElement('div', {}, `Device: ${deviceId}`), React.createElement('div', {}, `Status: ${status}`), React.createElement('div', {}, `Amount: €${amount}`));
            }
            else {
                inner = null;
            }
            return React.createElement('section', { className: 'payment-view' }, React.createElement('section', { className: 'payment-view-inner' }, inner), this.props.data && this.props.data.messages && React.createElement('ul', { className: 'messages' }, this.props.data.messages.map((m, key) => React.createElement('li', { key }, m.type))), React.createElement('div', {
                style: {
                    height: '60px', background: 'white', textAlign: 'center',
                    display: 'flex', justifyContent: 'center', alignItems: 'center',
                },
            }, this.props.data && React.createElement('img', {
                alt: '', style: { display: 'block', maxWidth: '100%', maxHeight: '100%' },
                src: base + `/payments/${this.props.data.paymentId}/barcode/png`,
            })));
        }
    }
    class Input extends React.Component {
        constructor() {
            super(...arguments);
            this.state = { amount: 0.0 };
            this.number = (e, num) => {
                if (this.props.disabled)
                    return;
                let amount = this.state.amount;
                if (num === '00') {
                    amount *= 100;
                }
                else if (num === 'backspace' || num === 'del') {
                    amount /= 10;
                }
                else {
                    amount = amount * 10 + parseInt(num, 10) / 100;
                }
                this.setState({ amount: Math.round(amount * 100) / 100 });
            };
            this.reset = () => {
                if (this.props.disabled)
                    return;
                this.setState({ amount: 0.0 });
            };
            this.ok = () => {
                if (this.props.disabled)
                    return;
                this.props.onSubmit(Object.assign({}, this.props.init, this.state));
            };
        }
        componentWillReceiveProps(nextProps) {
            if (this.props.init.orderId !== nextProps.init.orderId) {
                this.setState({ amount: 0.0 });
            }
        }
        render() {
            return React.createElement('section', { className: 'register' }, this.props.disabled && React.createElement('div', { className: `overlay` }), React.createElement('div', { className: `field` }, this.state.amount.toFixed(2)), React.createElement('section', { className: 'numbers' }, React.createElement(Button, { text: '1', onClick: this.number }), React.createElement(Button, { text: '2', onClick: this.number }), React.createElement(Button, { text: '3', onClick: this.number }), React.createElement(Button, { text: '4', onClick: this.number }), React.createElement(Button, { text: '5', onClick: this.number }), React.createElement(Button, { text: '6', onClick: this.number }), React.createElement(Button, { text: '7', onClick: this.number }), React.createElement(Button, { text: '8', onClick: this.number }), React.createElement(Button, { text: '9', onClick: this.number }), React.createElement(Button, { text: '0', onClick: this.number }), React.createElement(Button, { text: '00', onClick: this.number }), React.createElement(Button, { className: 'cancel', text: 'Reset', onClick: this.reset }), React.createElement(Button, { className: 'ok', text: 'OK', onClick: this.ok })));
        }
    }
    class Button extends React.Component {
        constructor() {
            super(...arguments);
            this.handleClick = () => {
                this.props.onClick.call(null, null, this.props.text);
            };
        }
        render() {
            return React.createElement('button', { onClick: this.handleClick, className: this.props.className }, React.createElement('span', { className: 'title' }, this.props.text));
        }
    }
    function parseJSON(response) {
        return response.json().then(json => ({
            json, status: response.status, ok: response.ok,
        }));
    }
    function getRandomRequest() {
        return {
            amount: 0.0,
            orderId: Math.floor((Math.random() * 20000000) + 1).toString(),
        };
    }
});
define("index", ["require", "exports", "react"], function (require, exports, React) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TournamentClient extends React.Component {
        constructor() {
            super(...arguments);
            this.state = {
                tournament: {
                    winnersRoot: 0,
                    losersRoot: 0,
                    tournamentPlayers: {},
                    dashboardFocus: null,
                    nodes: {},
                },
                pastVotes: {},
            };
            this.socket = null;
            this.connect = () => {
                clearTimeout(this.timeout);
                this.timeout = undefined;
                this.socket = new WebSocket('wss://api.tkolymp.cz/tournament/ws');
                this.socket.onmessage = e => {
                    const msg = JSON.parse(e.data);
                    if (msg['tag'] === 'StateMsg') {
                        this.setState({ tournament: msg.contents });
                    }
                    else {
                        localStorage.clear();
                    }
                };
                this.socket.onclose = () => {
                    this.timeout = this.timeout || setTimeout(this.connect, 500);
                };
                this.socket.onerror = () => {
                    this.timeout = this.timeout || setTimeout(this.connect, 500);
                };
                return this.socket;
            };
        }
        componentDidMount() {
            this.socket = this.connect();
        }
        render() {
            const t = this.state.tournament;
            return React.createElement("div", null,
                React.createElement("nav", { className: "navbar navbar-dark fixed-top", style: { backgroundColor: '#e80b30' } },
                    React.createElement("div", { className: "container" },
                        React.createElement("a", { className: "navbar-brand m-0", style: { lineHeight: '45px' } },
                            React.createElement("img", { src: "https://tkolymp.cz/style/new-logo-oneline.png", style: { height: '45px' }, className: "d-inline-block align-top" }),
                            "Tane\u010Dn\u00ED bitva o Olomouc"))),
                React.createElement("main", { className: 'container', style: { marginTop: '80px' } }, [2, 5, 8, 1].map((k, i) => React.createElement("div", { className: "mb-2" },
                    React.createElement("div", { className: "text-muted" },
                        "Battle #",
                        i + 1),
                    React.createElement(UserBattleComponent, { socket: this.socket, tournament: t, battle: t.nodes[k] }),
                    t.userFocus !== k ? null :
                        React.createElement(VoteComponent, { socket: this.socket, tournament: t, battle: t.nodes[k] })))));
        }
    }
    exports.TournamentClient = TournamentClient;
    class VoteComponent extends React.Component {
        constructor(props) {
            super(props);
            this.voteLeft = () => {
                if (!this.props.battle)
                    return null;
                const id = this.props.battle.contents[0];
                const pastVote = localStorage.getItem(id) || null;
                if (pastVote === 'left') {
                    this.props.socket.send(JSON.stringify([id, -1, 0]));
                    localStorage.removeItem(id);
                }
                else if (pastVote === 'right') {
                    this.props.socket.send(JSON.stringify([id, 1, -1]));
                    localStorage.setItem(id, 'left');
                }
                else if (pastVote === null) {
                    this.props.socket.send(JSON.stringify([id, 1, 0]));
                    localStorage.setItem(id, 'left');
                }
            };
            this.voteRight = () => {
                if (!this.props.battle)
                    return null;
                const id = this.props.battle.contents[0];
                const pastVote = localStorage.getItem(id) || null;
                if (pastVote === 'right') {
                    this.props.socket.send(JSON.stringify([id, 0, -1]));
                    localStorage.removeItem(id);
                }
                else if (pastVote === 'left') {
                    this.props.socket.send(JSON.stringify([id, -1, 1]));
                    localStorage.setItem(id, 'right');
                }
                else if (pastVote === null) {
                    this.props.socket.send(JSON.stringify([id, 0, 1]));
                    localStorage.setItem(id, 'right');
                }
            };
        }
        render() {
            if (!this.props.battle)
                return null;
            const n = this.props.battle;
            const t = this.props.tournament;
            const pastVote = localStorage.getItem(n.contents[0]) || null;
            const enabled = n.tag === 'DuelFinishedNode' && n.contents[1].victor === null;
            let message = null;
            let l = null, r = null;
            if (n.tag === 'DuelWaitingNode') {
                l = n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]] : null;
                r = n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]] : null;
                message = React.createElement("h3", { className: "text-center" }, "Pros\u00EDm po\u010Dkejte, hlasov\u00E1n\u00ED je\u0161t\u011B nebylo zah\u00E1jeno.");
            }
            else if (n.tag === 'DuelFinishedNode') {
                const res = n.contents[1];
                l = t.tournamentPlayers[res.leftPlayer] || {};
                r = t.tournamentPlayers[res.rightPlayer] || {};
                if (n.contents[1].victor !== null) {
                    message = React.createElement("h3", { className: "text-center" }, "Hlasov\u00E1n\u00ED ji\u017E bylo uzav\u0159eno.");
                }
            }
            return React.createElement("div", null,
                React.createElement("div", { className: "row m-2 mb-5" },
                    React.createElement("div", { className: "col-12" }, message),
                    React.createElement("div", { className: "col-8 mt-2 mb-2" },
                        React.createElement("button", { disabled: !enabled, onClick: this.voteLeft, className: 'btn btn-block text-left ' +
                                (pastVote === 'left' ? 'btn-primary' : 'btn-outline-primary') },
                            pastVote === 'left' ? '✓ ' : '',
                            l ? l.shortName : '?')),
                    React.createElement("div", { className: "col-8 offset-4" },
                        React.createElement("button", { disabled: !enabled, onClick: this.voteRight, className: 'btn btn-block text-right ' +
                                (pastVote === 'right' ? 'btn-primary' : 'btn-outline-primary') },
                            pastVote === 'right' ? '✓ ' : '',
                            r ? r.shortName : '?'))));
        }
    }
    exports.VoteComponent = VoteComponent;
    class UserBattleComponent extends React.Component {
        constructor(props) {
            super(props);
        }
        render() {
            const n = this.props.battle;
            const t = this.props.tournament;
            if (!n)
                return null;
            if (n.tag === 'DuelWaitingNode') {
                const l = n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]] : null;
                const r = n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]] : null;
                return React.createElement("ul", { className: "list-group" },
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" }, l ? l.longName : '?'),
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" }, r ? r.longName : '?'));
            }
            else if (n.tag === 'DuelFinishedNode') {
                const res = n.contents[1];
                const l = t.tournamentPlayers[res.leftPlayer] || {};
                const r = t.tournamentPlayers[res.rightPlayer] || {};
                return React.createElement("ul", { className: "list-group" },
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" },
                        React.createElement("span", { className: res.victor === res.leftPlayer ? 'font-weight-bold' : undefined }, l.longName),
                        React.createElement("span", { className: "badge badge-primary badge-pill" }, res.leftScore)),
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" },
                        React.createElement("span", { className: res.victor === res.rightPlayer ? 'font-weight-bold' : undefined }, r.longName),
                        React.createElement("span", { className: "badge badge-primary badge-pill" }, res.rightScore)));
            }
        }
    }
    exports.UserBattleComponent = UserBattleComponent;
});
//# sourceMappingURL=bundle.js.map