/* eslint-disable import-x/no-unused-modules */
import { cn } from '@/lib/cn';
import { publicPageMetadata } from '@/lib/server/seo';
import { PageHeader } from '@/ui/TitleBar';
import Image from 'next/image';

export const generateMetadata = () => publicPageMetadata({
  title: 'Galerie',
  description:
      'Fotografie a videa TK Olymp Olomouc ze soutěží, vystoupení, tréninků a klubových akcí na YouTube a Facebooku.',
  path: '/galerie',
});

export default function GalleryPage() {
  return (
    <>
      <PageHeader title="Galerie" />

      <div className="col-feature my-16 grid gap-16 px-4 md:grid-cols-2">
        {[
          {
            image: '/images/YouTube-Veverka.jpg',
            href: 'https://www.youtube.com/user/TheMamcro',
            label: (
              <>
                Videa
                <br />
                YouTube
              </>
            ),
          },
          {
            image: '/images/Facebook-202209.jpg',
            href: 'https://www.facebook.com/tkolymp/photos_albums?locale=cs_CZ',
            label: (
              <>
                Fotogalerie
                <br />
                Facebook
              </>
            ),
          },
        ].map((item) => (
          <a
            key={item.href}
            target="_blank"
            rel="noreferrer"
            href={item.href}
            className="relative aspect-w-16 aspect-h-9 group"
          >
            <div className="absolute inset-0 border-8 border-accent-7 overflow-hidden">
              <Image
                src={item.image}
                fill
                alt="Galerie TK Olymp"
                className="object-cover"
                sizes="(min-width: 768px) 430px, calc(100vw - 2rem)"
              />
            </div>
            <div
              className={cn(
                'absolute inset-0 flex justify-center items-center',
                'text-center text-2xl font-bold text-white',
                'bg-neutral-11/50 group-hover:bg-neutral-11/70',
              )}
            >
              {item.label}
            </div>
          </a>
        ))}
      </div>
    </>
  );
}
