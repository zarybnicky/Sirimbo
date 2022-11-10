import * as React from 'react';
import { Button, DialogTitle, Dialog } from '@mui/material';
import EmailIcon from '@mui/icons-material/Email';
import PhoneIcon from '@mui/icons-material/Phone';
import { UserFragment } from 'lib/graphql';

export const UserDetailButton: React.FC<{ user: UserFragment }> = ({ user }) => {
  const [open, setOpen] = React.useState(false);

  return <>
    <Button variant="text" sx={{ textTransform: 'none' }} onClick={() => setOpen(true)}>
      {user.uPrijmeni}, {user.uJmeno}
    </Button>

    <Dialog onClose={() => setOpen(false)} open={open}>
      <DialogTitle>{user.uJmeno} {user.uPrijmeni}</DialogTitle>
      <ul className="flex flex-col gap-3 m-4 mt-0">
        <li>
          <EmailIcon /> {user.uEmail}
        </li>
        <li>
          <PhoneIcon /> {user.uTelefon}
        </li>
      </ul>
    </Dialog>
  </>;
}
