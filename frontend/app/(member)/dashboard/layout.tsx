import type { ReactNode } from 'react';

export default function DashboardLayout({
  children,
  announcement,
}: {
  children: ReactNode;
  announcement: ReactNode;
}) {
  return (
    <>
      {children}
      {announcement}
    </>
  );
}
