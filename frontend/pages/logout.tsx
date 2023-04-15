import React from 'react';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { Layout } from 'components/layout/Layout';

export default function LogoutPage() {
  const { signOut } = useAuth();
  const router = useRouter();
  React.useEffect(() => {
    signOut();
    router.push('/');
  }, [router, signOut]);
  return null;
}

LogoutPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
