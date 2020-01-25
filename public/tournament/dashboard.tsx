import * as React from 'react';

interface Payment {
  paymentId: string;
  created: Date;
  deviceId: string;
  amount: number;
  orderId: number;
  status: string;
  messages: DeviceMessage[];
}
interface DeviceMessage {
  type: MessageType;
  warnings: string[];
  error?: string;
  raw: string;
}
enum MessageType {
  Request = 'Request',
  ResponseSuccess = 'ResponseSuccess',
  ResponseStatus = 'ResponseStatus',
  ResponseError = 'ResponseError',
  ResponseUnknown = 'ResponseUnknown',
}
interface Device {
  id: string;
  name: string;
  status: DeviceStatus;
}
enum DeviceStatus {
  Disconnected = 'Disconnected',
  Available = 'Available',
  Busy = 'Busy',
}

interface BalieRequest {
  amount: number;
  orderId: string;
}
interface BaliePaymentStatus {
  busy: boolean;
  error?: string;
  data?: Payment;
}

interface AppState {
  request: BalieRequest;
  paymentStatus: BaliePaymentStatus;
  devices: Device[];
  types: string[];
  selectedDevice?: string;
}

const base = location.origin;

export class TournamentDashboard extends React.Component<null, AppState> {
  state = {
    request: getRandomRequest(),
    paymentStatus: { busy: false },
    devices: [], types: [],
    selectedDevice: undefined,
  };
  editStatus = (x: Partial<BaliePaymentStatus>) => {
    this.setState({ paymentStatus: Object.assign({}, this.state.paymentStatus, x) });
  }
  fetchDevices = () => {
    fetch(base + '/devices/connected').then(parseJSON).then((res) => {
      if (!res.ok) {
        throw res;
      }
      this.setState({
        devices: res.json.filter((x: Device) => x.status !== 'Disconnected'),
      });
    });
  }
  fetchTypes = () => {
    fetch(base + '/types').then(parseJSON).then((res) => {
      if (!res.ok) {
        throw res;
      }
      this.setState({ types: res.json });
    });
  }

  componentWillMount() {
    setInterval(this.fetchDevices, 1500);
    setInterval(this.fetchTypes, 1500);
    this.fetchDevices();
    this.fetchTypes();
  }

  onSubmit = (req: BalieRequest) => {
    if (this.state.selectedDevice === undefined) {
      return;
    }

    this.editStatus({ busy: true });
    let timerId: number | undefined;

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
      this.editStatus({ data: { ...res.json, paymentId: res.json.paymentInfo.paymentId } });
      timerId = setInterval(() => this.onRefresh(res.json.paymentInfo.paymentId, timerId), 1000);
    }).catch((error) => {
      console.log(error);
      this.editStatus({ error, busy: false });
      timerId && clearInterval(timerId);
    });
  }

  onRefresh = (id: string, timerId?: number) => {
    fetch(base + `/payments/${id}`).then(parseJSON).then((res) => {
      if (!res.ok) {
        throw res;
      }
      const data = res.json;
      if (data.status !== 'pending') {
        this.editStatus({ data, busy: false });
        timerId && clearInterval(timerId);
        this.setState({ request: getRandomRequest() });
      } else {
        this.editStatus({ data });
      }
    }).catch((error) => {
      console.log(error);
      this.editStatus({ error, busy: false });
      timerId && clearInterval(timerId);
    });
  }

  onDevice = (x: string) => this.setState({ selectedDevice: x });

  render() {
    return React.createElement(
      'main', { className: 'app' },
      React.createElement(
        'header', {},
        React.createElement(
          'div', {},
          this.state.types.find(x => x === 'Yomani') &&
          React.createElement('a', { href: '/yomani.html', target: '_blank' }, 'Mock Yomani'),
          ' ',
          this.state.types.find(x => x === 'Kiosk') &&
          React.createElement('a', { href: '/kiosk.html', target: '_blank' }, 'Mock Kiosk'),
        ),
        React.createElement(
          'div', { style: { fontWeight: 'bold' } },
          `New sale, sale no. ${this.state.request.orderId}`,
        ),
      ),
      React.createElement(
        'div', { className: 'app-inner' },
        React.createElement(DeviceSelector, {
          devices: this.state.devices,
          selected: this.state.selectedDevice,
          onDevice: this.onDevice,
        }),
        React.createElement(Input, {
          onSubmit: this.onSubmit,
          init: this.state.request,
          disabled: this.state.paymentStatus.busy,
        }),
        React.createElement(PaymentView, this.state.paymentStatus),
      ),
    );
  }
}

interface DeviceSelectorProps {
  devices: Device[];
  selected?: string;
  onDevice: (x: string) => void;
}
class DeviceSelector extends React.Component<DeviceSelectorProps> {
  render() {
    return React.createElement(
      'ul', { className: 'device-selector' },
      this.props.devices.map(x => React.createElement(
        'li', {
          key: x.id,
          className: x.id === this.props.selected ? 'selected' : null,
          onClick: () => this.props.onDevice(x.id),
        },
        x.name,
      )),
    );
  }
}

class PaymentView extends React.Component<BaliePaymentStatus> {
  render() {
    let inner;
    if (this.props.error) {
      inner = `Error: ${JSON.stringify(this.props.error)}`;
    } else if (this.props.data) {
      const { paymentId, deviceId, amount, status, messages } = this.props.data;
      inner = React.createElement(
        'div', {},
        React.createElement('div', {}, `Payment: ${paymentId}`),
        React.createElement('div', {}, `Device: ${deviceId}`),
        React.createElement('div', {}, `Status: ${status}`),
        React.createElement('div', {}, `Amount: €${amount}`),
      );
    } else {
      inner = null;
    }

    return React.createElement(
      'section', { className: 'payment-view' },
      React.createElement('section', { className: 'payment-view-inner' }, inner),
      this.props.data && this.props.data.messages && React.createElement(
        'ul', { className: 'messages' },
        this.props.data.messages.map(
          (m, key) => React.createElement('li', { key }, m.type)),
      ),
      React.createElement(
        'div', {
          style: {
            height: '60px', background: 'white', textAlign: 'center',
            display: 'flex', justifyContent: 'center', alignItems: 'center',
          },
        },
        this.props.data && React.createElement('img', {
          alt: '', style: { display: 'block', maxWidth: '100%', maxHeight: '100%' },
          src: base + `/payments/${this.props.data.paymentId}/barcode/png`,
        }),
      ),
    );
  }
}

interface InputProps {
  disabled: boolean;
  onSubmit: (x: BalieRequest) => void;
  init: BalieRequest;
}
class Input extends React.Component<InputProps, { amount: number }> {
  state = { amount: 0.0 };

  componentWillReceiveProps(nextProps: InputProps) {
    if (this.props.init.orderId !== nextProps.init.orderId) {
      this.setState({ amount: 0.0 });
    }
  }

  number = (e: any, num: string) => {
    if (this.props.disabled) return;

    let amount = this.state.amount;
    if (num === '00') {
      amount *= 100;
    } else if (num === 'backspace' || num === 'del') {
      amount /= 10;
    } else {
      amount = amount * 10 + parseInt(num, 10) / 100;
    }
    this.setState({ amount: Math.round(amount * 100) / 100 });
  }
  reset = () => {
    if (this.props.disabled) return;
    this.setState({ amount: 0.0 });
  }
  ok = () => {
    if (this.props.disabled) return;
    this.props.onSubmit(Object.assign({}, this.props.init, this.state));
  }
  render() {
    return React.createElement(
      'section', { className: 'register' },
      this.props.disabled && React.createElement('div', { className: `overlay` }),
      React.createElement('div', { className: `field` }, this.state.amount.toFixed(2)),
      React.createElement(
        'section', { className: 'numbers' },
        React.createElement(Button, { text: '1', onClick: this.number }),
        React.createElement(Button, { text: '2', onClick: this.number }),
        React.createElement(Button, { text: '3', onClick: this.number }),
        React.createElement(Button, { text: '4', onClick: this.number }),
        React.createElement(Button, { text: '5', onClick: this.number }),
        React.createElement(Button, { text: '6', onClick: this.number }),
        React.createElement(Button, { text: '7', onClick: this.number }),
        React.createElement(Button, { text: '8', onClick: this.number }),
        React.createElement(Button, { text: '9', onClick: this.number }),
        React.createElement(Button, { text: '0', onClick: this.number }),
        React.createElement(Button, { text: '00', onClick: this.number }),
        React.createElement(Button, { className: 'cancel', text: 'Reset', onClick: this.reset }),
        React.createElement(Button, { className: 'ok', text: 'OK', onClick: this.ok }),
      ),
    );
  }
}

interface ButtonProps {
  className?: string;
  text: string;
  onClick: (e: any, t: string) => void;
}
class Button extends React.Component<ButtonProps> {
  handleClick = () => {
    this.props.onClick.call(null, null, this.props.text);
  }
  render() {
    return React.createElement(
      'button', { onClick: this.handleClick, className: this.props.className },
      React.createElement('span', { className: 'title' }, this.props.text),
    );
  }
}

function parseJSON(response: Response) {
  return response.json().then(json => ({
    json, status: response.status, ok: response.ok,
  }));
}

function getRandomRequest(): BalieRequest {
  return {
    amount: 0.0,
    orderId: Math.floor((Math.random() * 20000000) + 1).toString(),
  };
}
