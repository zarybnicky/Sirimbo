import React from 'react';
import Link from 'next/link';
import Image from 'next/image';
import LogoOnDark from './logo-white.webp';
import LogoOnWhite from './logo-vec.webp';

export function SidebarLogo() {
  return (
    <div className="hidden lg:flex">
      <Link href="/dashboard" aria-label="Nástěnka" className="h-16 mt-3 mx-auto">
        <Image alt="" src={LogoOnDark} loading="eager" sizes="192px" />
      </Link>
    </div>
  );
}

export function MobileLogo() {
  return <Image alt="" src={LogoOnWhite} loading="eager" height="50" />;
}
