import React from 'react';
import clsx from 'clsx';
import { View, Navigate } from './utils/constants';
import VIEWS from './Views';

const Toolbar: React.FC<{
  onNavigate: (x: Navigate) => void;
  onView: (x: View) => void;
  view: View;
  label: React.ReactNode;
}> = ({ onNavigate, onView, view, label }) => (
  <div className="rbc-toolbar">
    <span className="rbc-btn-group">
      <button type="button" onClick={() => onNavigate(Navigate.TODAY)}>
        Dnes
      </button>
      <button type="button" onClick={() => onNavigate(Navigate.PREVIOUS)}>
        Zpět
      </button>
      <button type="button" onClick={() => onNavigate(Navigate.NEXT)}>
        Dále
      </button>
    </span>

    <span className="rbc-toolbar-label">{label}</span>

    <span className="rbc-btn-group">
      {Object.values(View).map((name) => (
        <button
          type="button"
          key={name}
          className={clsx({ 'rbc-active': view === name })}
          onClick={onView.bind(null, name)}
        >
          {VIEWS[name].name}
        </button>
      ))}
    </span>
  </div>
);

export default Toolbar;
