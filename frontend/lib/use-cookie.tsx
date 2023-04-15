import React from 'react';
import { getCookie, setCookies } from 'cookies-next';

export const useCookie = (key: string, defaultValue: string | undefined = undefined) => {
  const [cookie, setCookie] = React.useState<string | undefined>(defaultValue);

  React.useEffect(() => {
    setCookie(getCookie(key) as string);
  }, [key]);

  const updateCookie = (value: string) => {
    const expires = new Date();
    const numberOfDays = 30;
    expires.setTime(expires.getTime() + numberOfDays * 60 * 60 * 24 * 1000);

    setCookie(value);
    setCookies(key, value, { path: '/', expires });
  };

  return [cookie, updateCookie] as const;
};
