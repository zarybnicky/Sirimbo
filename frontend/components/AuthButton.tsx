import * as React from 'react';
import { useRouter } from 'next/router';
import { useAuth } from 'lib/data/use-auth';
import { User as Account } from 'react-feather';
import Link from 'next/link';
import { Dropdown } from './Dropdown';

export const AuthButton = ({ }) => {
  const auth = useAuth();
  const router = useRouter();
  const signOut = React.useCallback(() => {
    auth.signOut();
    router.push('/');
  }, [router, auth]);

  if (!auth.user) {
    return <Link href="/login" passHref>
      <a className="button button-text">
        <Account /> <span className="underline">Přihlásit</span>
      </a>
    </Link>;
  }
  const button = (
    <div className="flex normal-case button button-text gap-2 items-center">
      <Account />
      <div className="flex flex-col items-start" style={{ lineHeight: 1.3 }}>
        <span className="text-sm uppercase underline">Přihlášen</span>
        <span className="text-sm">{auth.user?.uJmeno} {auth.user?.uPrijmeni}</span>
      </div>
    </div>
  );
  return <Dropdown align="end" button={button}
    options={[{ title: 'Odhlásit se', onClick: signOut }]}
  />;
};
