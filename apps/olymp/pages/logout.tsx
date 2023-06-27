import React from 'react';
import { useAuth } from '@app/ui/use-auth';
import { useRouter } from 'next/router';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const { signOut } = useAuth();
  const router = useRouter();
  React.useEffect(() => {
    void (async () => {
      await signOut();
      await router.push('/');
    })();
  }, [router, signOut]);
  return null;
}

Page.showTopMenu = true;

export default Page;
