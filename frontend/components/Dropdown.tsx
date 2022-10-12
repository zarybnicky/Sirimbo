import * as React from 'react';
import { Button, Menu, MenuItem } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { NextLinkComposed } from './Link';

export const Dropdown: React.FC<{
  button: JSX.Element;
  options: {
    title: string;
    href?: string;
    onClick?: () => void;
  }[];
}> = ({ button, options }) => {
  return <PopupState variant="popover" popupId="demoMenu">
    {(popupState) => <>
      <Button {...bindTrigger(popupState)} color="inherit">
        {button}
      </Button>
      <Menu
        {...bindMenu(popupState)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
        transformOrigin={{ vertical: 'top', horizontal: 'left' }}
      >
        {options.map((option, i) => option.href ? (
          <MenuItem key={i} component={NextLinkComposed} href={option.href}>{option.title}</MenuItem>
        ) : (
          <MenuItem key={i} onClick={option.onClick}>{option.title}</MenuItem>
        ))}
      </Menu>
    </>}
  </PopupState>;
};
