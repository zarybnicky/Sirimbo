import React from "react";
import { getCookie, setCookie } from "cookies-next";

export const useCookie = (key: string, defaultValue: string | undefined = undefined) => {
  const [cookie, setCookieLocal] = React.useState<string | undefined>((getCookie(key) as string) ?? defaultValue);

  React.useEffect(() => {
    setCookieLocal((getCookie(key) as string));
  }, [key]);

  const updateCookie = React.useCallback((value: string, numberOfDays: number) => {
    const expires = new Date();
    expires.setTime(expires.getTime() + numberOfDays * 60 * 60 * 24 * 1000);

    setCookieLocal(value);
    setCookie(key, value, { path: '/', expires });
  }, [key]);

  const refresh = React.useCallback(() => {
    setCookieLocal((getCookie(key) as string) ?? defaultValue);
  }, [key, defaultValue]);

  return [cookie, updateCookie, refresh] as const;
};
