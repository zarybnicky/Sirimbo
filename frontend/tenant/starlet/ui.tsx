import React from 'react';
import Link from 'next/link';
import Image from 'next/image';
import LogoIcon from './logo-white-no-text.png';

export function SidebarLogo() {
  return (
    <div className="hidden lg:flex">
      <Link
        href="/dashboard"
        aria-label="Nástěnka"
        className="h-28 pt-3 pl-2 pr-4 mx-auto"
      >
        <Image alt="" src={LogoIcon} sizes="256px" />
      </Link>
    </div>
  );
}

export function MobileLogo() {
  return <Image className="pb-1 pr-4 -mt-1" alt="" src={LogoIcon} height="65" />;
}
