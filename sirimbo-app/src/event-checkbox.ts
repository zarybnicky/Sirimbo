import { LitElement, html, css } from 'lit';
import { property, customElement } from 'lit/decorators.js';
import { styleMap } from 'lit/directives/style-map.js';

declare global {
  interface HTMLElementTagNameMap {
    'event-checkbox': EventCheckbox;
  }
}

@customElement('event-checkbox')
export class EventCheckbox extends LitElement {
  static override styles = css`
    .user-card {
      font-family: 'Arial', sans-serif;
      background: #f4f4f4;
      width: 500px;
      display: grid;
      grid-template-columns: 1fr 2fr;
      grid-gap: 10px;
      margin-bottom: 15px;
      border-bottom: blue 5px solid;
    }

    img {
      width: 100%;
    }

    button {
      cursor: pointer;
      background: blue;
      color: #fff;
      border: 0;
      border-radius: 5px;
      padding: 5px 10px;
    }

    slot[name='email'] {
      color: blue;
    }
  `;

  @property({ type: String })
  name = 'Default';

  @property()
  image = '';

  @property({ type: Boolean })
  showInfo = true;

  infoStyle = { display: 'block' };

  override render() {
    return html`
      <div class="user-card">
        <h1>Hello</h1>
        <img src=${this.image} />
        <div>
          <h3>${this.name}</h3>
          <div id="info" style=${styleMap(this.infoStyle)}>
            <slot name="email"></slot>
            <slot name="phone"></slot>
          </div>
          <button @click=${this.handleClick}>
            ${this.showInfo ? 'Hide Info' : 'Show Info'}
          </button>
        </div>
      </div>
    `;
  }

  private handleClick() {
    this.showInfo = !this.showInfo;

    if (this.showInfo) {
      this.infoStyle = { display: 'block' };
    } else {
      this.infoStyle = { display: 'none' };
    }
  }
}
