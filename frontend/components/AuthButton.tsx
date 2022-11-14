import * as React from 'react';
import { useRouter } from 'next/router';
import { Menu, MenuItem } from '@mui/material';
import { PopupState as PopupStateType } from 'material-ui-popup-state/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { useAuth } from 'lib/data/use-auth';
import AccountCircle from '@mui/icons-material/AccountCircle';
import Link from 'next/link';

export const AuthButton = ({ }) => {
  const auth = useAuth();
  const router = useRouter();

  const signOut = (popupState: PopupStateType) => {
    popupState.close();
    auth.signOut();
    router.push('/');
  };

  if (!auth.user) {
    return <Link href="/login" passHref>
      <a className="button button-text">
        <AccountCircle /> <span className="underline">Přihlásit</span>
      </a>
    </Link>;
  }
  return <PopupState variant="popover" popupId="demoMenu">
    {(popupState) => <>
      <button className="flex normal-case button button-text gap-2 items-center" {...bindTrigger(popupState)}>
        <AccountCircle />
        <div className="flex flex-col items-start" style={{ lineHeight: 1.3 }}>
          <span className="text-sm uppercase underline">Přihlášen</span>
          <span className="text-sm">{auth.user?.uJmeno} {auth.user?.uPrijmeni}</span>
        </div>
      </button>
      <Menu
        {...bindMenu(popupState)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
      >
        <MenuItem onClick={() => signOut(popupState)}>Odhlásit se</MenuItem>
      </Menu>
    </>}
  </PopupState>;
};
