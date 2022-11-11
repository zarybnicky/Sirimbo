import * as React from 'react';
import { useRouter } from 'next/router';
import { Menu, MenuItem } from '@mui/material';
import { PopupState as PopupStateType } from 'material-ui-popup-state/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { useAuth } from 'lib/data/use-auth';
import AccountCircle from '@mui/icons-material/AccountCircle';
import { Button } from './Button';

export const AuthButton = ({ }) => {
  const auth = useAuth();
  const router = useRouter();

  const signOut = (popupState: PopupStateType) => {
    popupState.close();
    auth.signOut();
    router.push('/');
  };

  if (!auth.user) {
    return <div>
      <Button href="/login" className="underline">
        <AccountCircle /> Přihlásit
      </Button>
    </div>;
  }
  return <PopupState variant="popover" popupId="demoMenu">
    {(popupState) => <>
      <Button
        {...bindTrigger(popupState)}
      >
        <AccountCircle />
        <div style={{ display: 'flex', alignItems: 'start', lineHeight: 1.3, flexDirection: 'column' }}>
          <span style={{ textDecoration: 'underline' }}>Přihlášen</span>
          <span style={{ textTransform: 'none' }}>{auth.user?.uJmeno} {auth.user?.uPrijmeni}</span>
        </div>
      </Button>
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
