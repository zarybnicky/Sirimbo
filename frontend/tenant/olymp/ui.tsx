import * as React from 'react';
import Link from 'next/link';
import SiFacebook from '@icons-pack/react-simple-icons/icons/SiFacebook';
import SiInstagram from '@icons-pack/react-simple-icons/icons/SiInstagram';
import SiYoutube from '@icons-pack/react-simple-icons/icons/SiYoutube';
import LogoCsts from './logo-csts.svg';
import LogoNsa from './logo-nsa.svg';
import LogoProstejov from './logo-prostejov.svg';
import LogoOlomouc from './logo-olomouc.jpg';
import LogoKraj from './logo-kraj.png';
import tenantConfig from './config.js';
import { DefaultSeo } from 'next-seo';
import LogoOnDark from './logo.webp';
import Image from 'next/image';

export function TenantSeo() {
  return <DefaultSeo
    titleTemplate={`%s · ${tenantConfig.shortName}`}
    defaultTitle={tenantConfig.shortName}
    themeColor="#000"
    facebook={{ appId: '704526480597551' }}
    openGraph={{ siteName: tenantConfig.shortName }}
    additionalMetaTags={[
      { name: "wot-verification", content: "ec0cf41ab42dae52d3d4" },
      { name: "msvalidate.01", content: "7BD6C8B5748FC22EF06AB3AE89900885" },
      { name: "facebook-domain-verification", content: "k8tt64a93roxiymxo79clpvklan9j2" },
      { name: "google-site-verification", content: "Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w" },
      { name: "norton-safeweb-site-verification", content: "r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" },
      { name: 'viewport', content: 'initial-scale=1,width=device-width' },
    ]}

    additionalLinkTags={[
      {
        rel: 'apple-touch-icon',
        sizes: '180x180',
        href: '/olymp/apple-touch-icon.png?v=3',
      },
      { rel: 'icon', sizes: '32x32', href: '/olymp/favicon-32x32.png?v=3' },
      { rel: 'icon', sizes: '16x16', href: '/olymp/favicon-16x16.png?v=3' },
      { rel: 'shortcut icon', href: '/olymp/favicon.ico?v=3' },
      { rel: 'manifest', href: '/olymp/site.webmanifest?v=3' },
      {
        rel: 'mask-icon',
        color: '#5bbad5',
        href: '/olymp/safari-pinned-tab.svg?v=3',
      },
    ]}
  />
}

function OlympLogoVertical(props: React.SVGProps<SVGSVGElement>) {
  return <svg
    fill="currentColor"
    width={173.04}
    height={174.65}
    viewBox="0 0 45.784 46.209"
    {...props}
  >
    <path
      strokeWidth={0.352778}
      stroke="none"
      fillRule="evenodd"
      transform="translate(-89.745 -64.586)"
      d="M109.232 95.354c5.543-10.512 7.858-17.427 17.018-19.946l-.172-.273c-.028-.034-.122-.17-.15-.205-9.71.812-14.55 1.232-24.632.06-.088.141-.173.286-.257.432l.034.007c3.365.77 9.204 1.287 10.986 4.338 2.407 3.672-2.39 11.176-4.15 14.927.399.21.907.482 1.323.66"
    />
    <path
      strokeWidth={0.352778}
      stroke="none"
      fillRule="evenodd"
      transform="translate(-89.745 -64.586)"
      d="M106.503 93.757c2.18-3.608 6.454-10.567 3.993-13.92-1.603 3.314-5.832 4.087-11.817 4.227.922 3.47 4.826 7.57 7.824 9.693M111.654 70.57a1.912 1.912 0 1 1 .186 3.819 1.912 1.912 0 0 1-.186-3.82M118.026 69.589a2.393 2.393 0 1 1 .233 4.78 2.393 2.393 0 0 1-.233-4.78"
    />
    <path
      strokeWidth={0.352778}
      stroke="none"
      fillRule="evenodd"
      transform="translate(-89.745 -64.586)"
      d="M94.745 102.02h1.396v3.721h1.19v-3.72h1.41v-.962h-3.996zm4.683 3.721h1.189v-1.149l.62-.687 1.236 1.836h1.424l-1.851-2.738 1.736-1.937h-1.375l-1.79 2.024v-2.024h-1.19zm6.412-2.344c0 1.369 1.081 2.398 2.524 2.398 1.45 0 2.533-1.035 2.533-2.398 0-1.356-1.083-2.371-2.533-2.371-1.443 0-2.524 1.009-2.524 2.371zm1.215.007c0-.788.602-1.383 1.323-1.383.722 0 1.297.595 1.297 1.383 0 .789-.575 1.396-1.297 1.396-.715 0-1.323-.607-1.323-1.396zm4.65 2.338h3.173v-.995h-1.984v-3.681h-1.19zm3.42-4.676 1.73 3.2v1.476h1.182v-1.51l1.696-3.166h-1.188l-1.102 2.037-1.13-2.037zm5.223 4.676h1.062v-3.039l1.122 2.571h.776l1.121-2.571.007 3.039h1.05v-4.683h-1.298l-1.269 2.785-1.269-2.785h-1.302zm6.259 0h1.189v-1.316h.849c1.195 0 1.884-.634 1.884-1.73 0-1.043-.69-1.63-1.884-1.63h-2.038zm1.189-2.25v-1.49h.788c.521 0 .822.246.822.734 0 .493-.3.755-.822.755zm0 0"
    />
  </svg>
}

export function MobileLogo() {
  return (
    <Image alt="" src={LogoOnDark} height="50" priority />
  );
}

export function DesktopLogo() {
  return (
    <div className="relative overflow-visible min-w-[104px]">
      <div className="w-[104px] h-[130px] text-white bg-primary z-30 shadow-accent-9/60 shadow-lg absolute top-0 inset-x-0">
        <Link href="/" className="block p-0 m-0 size-full relative">
          <OlympLogoVertical
            style={{
              filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
              position: 'absolute',
              left: 0,
              bottom: 0,
              width: '104px',
              height: '104px',
              color: 'white',
              fill: 'white !important',
            }}
          />
        </Link>
      </div>
    </div>
  );
}

export function SidebarLogo() {
  return (
    <div className="hidden lg:flex">
      <Link href="/" className="h-20 mt-3 mx-auto">
        <OlympLogoVertical className="size-full text-white !fill-white" />
      </Link>
    </div>
  );
}

export function SocialIcons() {
  return (
    <div className="flex gap-1 items-center">
      <a
        target="_blank"
        rel="noreferrer"
        href="https://www.facebook.com/tkolymp"
        className="p-1"
      >
        <SiFacebook className="text-accent-10" />
      </a>
      <a
        target="_blank"
        rel="noreferrer"
        href="https://www.instagram.com/tanecni_klub_olymp"
        className="p-1"
      >
        <SiInstagram className="text-accent-9" />
      </a>
      <a
        target="_blank"
        rel="noreferrer"
        href="https://www.youtube.com/user/TheMamcro"
        className="p-1"
      >
        <SiYoutube className="text-white" />
      </a>
    </div>
  );
}

export function Sponsors() {
  return (
    <div className="flex flex-wrap lg:flex-nowrap m-4 gap-4 items-stretch justify-center text-center text-neutral-11">
      {[
        { label: 'Český svaz tanečního sportu', image: LogoCsts.src },
        { label: 'Město Olomouc', image: LogoOlomouc.src },
        { label: 'Olomoucký kraj', image: LogoKraj.src },
        { label: 'Město Prostějov', image: LogoProstejov.src },
        { label: 'Národní sportovní agentura', image: LogoNsa.src },
      ].map((x) => (
        <div key={x.label} className="flex flex-col grow">
          <div className="grow flex items-center">
            <img alt={x.label} className="w-full h-auto" src={x.image} />
          </div>
          <div className="h-12 mt-4">{x.label}</div>
        </div>
      ))}
    </div>
  );
}
