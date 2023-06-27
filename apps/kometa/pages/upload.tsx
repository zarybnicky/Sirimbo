import { LoginForm } from 'components/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import { Dropzone } from '@app/upload/Dropzone';

export default function UploadPage() {
  const { user } = useAuth();
  if (!user) {
    return <LoginForm />;
  }

  return <Dropzone />;
}
