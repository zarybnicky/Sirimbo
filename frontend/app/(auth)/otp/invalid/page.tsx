import { StatusPage } from '@/ui/StatusPage';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Neplatný odkaz',
};

export default function InvalidOtpPage() {
  return <StatusPage status="error" title="Použitý odkaz již vypršel nebo je neplatný." details={null} />;
}
