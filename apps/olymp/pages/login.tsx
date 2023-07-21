import { LoginForm } from '@app/ui/LoginForm';
import { useRouter } from 'next/router';
import type { NextPageWithLayout } from 'pages/_app';
import * as React from 'react';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const onSuccess = React.useCallback(() => {
    const redirect = router.query?.from as string | undefined;
    router.push(redirect || '/dashboard');
  }, [router])
  return <LoginForm onSuccess={onSuccess} />
}

Page.staticTitle = "Přihlášení";
Page.requireLoggedOut = true;

export default Page;
