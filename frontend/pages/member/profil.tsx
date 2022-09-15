import * as React from 'react';
import { useAuth } from 'lib/data/use-auth';
import { PhpPageView, PhpPage, getServerSidePhpPage } from 'components/PhpDynamicPage';
import { useRouter } from 'next/router';
import { useSnackbar } from 'notistack';

export const ProfilePage: React.FC<{ page: PhpPage; }> = ({ page }) => {
  const { user, isLoading } = useAuth();
  const router = useRouter();
  const { enqueueSnackbar } = useSnackbar();
  if (!user && !isLoading) {
    enqueueSnackbar('Nejprve se musíte přihlásit', { variant: 'error' })
    router.push('/login');
  }
  return <PhpPageView page={page} />
}

export default ProfilePage;
export const getServerSideProps = getServerSidePhpPage;
