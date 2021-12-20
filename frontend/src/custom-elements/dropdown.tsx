import * as React from 'react';

export const Dropdown = ({ links }: { links: { [k: string]: string; }; }) => (
  <div className="btn-group">
    <button type="button" className="btn btn-xs pt-0" data-toggle="dropdown">
      <img alt="Upravit" width="14" src="/style/icon-gear.png" />
    </button>
    <div className="dropdown-menu">
      {Object.keys(links).map(url => <a className="dropdown-item" href={url}>{links[url]}</a>)}
    </div>
  </div>
);
