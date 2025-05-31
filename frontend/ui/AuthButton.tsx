"use client";

import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuLabel,
  DropdownMenuLink,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { useAuth } from '@/ui/use-auth';
import { memberMenu } from '@/lib/use-menu';
import { User as Account } from 'lucide-react';
import React from 'react';
import { useSetAtom } from 'jotai';
import { authAtom, storeRef } from '@/ui/state/auth';
import Link from 'next/link';

export function AuthButton() {
  const auth = useAuth();
  const setAuth = useSetAtom(authAtom);

  const [isMounted, setIsMounted] = React.useState(false);
  React.useEffect(() => {
    setIsMounted(true);
  }, []);

  const signOut = React.useCallback(() => {
    setAuth(null, null);
    storeRef.resetUrqlClient?.();
  }, [setAuth]);

  if (!auth.user || !isMounted) {
    return (
      <Link href="/login" className="flex items-center gap-2 uppercase font-bold text-sm">
        <Account className="size-4" />
        Pro členy
      </Link>
    );
  }

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className="min-h-[48px] md:min-h-[64px] flex gap-2 items-center drop-shadow">
        <Account className="size-4" />
        <div
          className="flex flex-col justify-center items-start"
          style={{ lineHeight: 1.3 }}
        >
          <span className="text-xs uppercase tracking-wider">Přihlášen</span>
          <span className="text-sm font-normal">{auth.user?.uEmail}</span>
        </div>
      </DropdownMenuTrigger>

      <DropdownMenuContent align="end">
        {memberMenu.map((item) =>
          item.type === 'link' ? (
            <DropdownMenuLink key={JSON.stringify(item.href)} href={item.href}>
              {item.title}
            </DropdownMenuLink>
          ) : (
            <React.Fragment key={item.title}>
              <DropdownMenuLabel>{item.title}</DropdownMenuLabel>
              {item.children.filter((item) => (!item.requireTrainer || auth.isTrainerOrAdmin) && (!item.requireAdmin || auth.isAdmin)).map((item) => (
                <DropdownMenuLink key={JSON.stringify(item.href)} href={item.href}>
                  {item.title}
                </DropdownMenuLink>
              ))}
            </React.Fragment>
          ))}
        <DropdownMenuButton onClick={signOut}>Odhlásit se</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
