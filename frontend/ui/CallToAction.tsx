import { ChevronRight } from 'lucide-react';
import { buttonCls } from '@/ui/style';
import CallToAction1 from '@/tenant/olymp/call-to-action-1.webp';
import CallToAction2 from '@/tenant/olymp/call-to-action-2.webp';
import CallToAction3 from '@/tenant/olymp/call-to-action-3.webp';
import CallToAction4 from '@/tenant/olymp/call-to-action-4.webp';

const images = [
  CallToAction1.src,
  CallToAction2.src,
  CallToAction3.src,
  CallToAction4.src,
];

export function CallToAction({ url }: { url: string }) {
  const image =
    images[
      [...url].map((x) => x.codePointAt(0) || 0).reduce((a, b) => a + b * 3, 0) %
        images.length
    ]!;

  return (
    <div
      className="col-full-width content mt-4 bg-accent-7"
      style={{
        backgroundImage: `linear-gradient(90deg, rgba(216,28,58,0.8) 30%, rgba(255,255,255,0) 50%), url(${image})`,
        backgroundPosition: 'left, 65% 50%',
        backgroundRepeat: 'no-repeat, no-repeat',
        backgroundSize: '100%, auto 100%',
      }}
    >
      <div className="col-feature px-4 py-16 md:px-8 md:pr-24">
        <div className="text-white font-black text-3xl md:text-4xl">PŘIDEJ SE K NÁM</div>

        <div className="font-bold text-2xl md:text-3xl [text-shadow:0_0_6px_rgba(255,255,255,0.8)]">
          A OBJEV LÁSKU K TANCI
        </div>

        <a
          href="https://nabor.tkolymp.cz"
          className={buttonCls({ size: 'lg', variant: 'outlineDark', className: 'mt-4' })}
        >
          Chci tančit
          <ChevronRight className="mt-0.5" />
        </a>
      </div>
    </div>
  );
}
