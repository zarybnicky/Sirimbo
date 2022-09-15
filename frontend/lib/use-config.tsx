import { useCookie } from "lib/use-cookie";
import React from "react";

type ConfigContext = {
  layout: 'new' | 'old';
  setLayout: (layout: 'new' | 'old') => void;
}

const ConfigContext = React.createContext<ConfigContext | undefined>(undefined);

export const useConfig = (): ConfigContext => {
  const ctx = React.useContext(ConfigContext);
  if (ctx === undefined) {
    throw new Error('useConfig must be called from within ConfigProvider');
  }
  return ctx;
};

export const ConfigProvider: React.FC = ({ children }) => {
  let [layout, setLayout] = useCookie('layout', 'old');
  const ctx = {
    layout: (layout === 'new' || layout === 'old') ? layout as 'old' | 'new' : 'old',
    setLayout,
  };
  return (
    <ConfigContext.Provider value={ctx}>
      {children}
    </ConfigContext.Provider>
  );
};
