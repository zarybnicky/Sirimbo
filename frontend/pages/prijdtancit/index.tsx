import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import { buttonCls } from '@/ui/style';
import Link from 'next/link';
import { ChevronRight } from 'lucide-react';
import Image from 'next/image';

const Page = () => {
  return (
    <Layout showTopMenu>
      <NextSeo title="Přijď tančit!" />

      <div className="col-feature mt-8 flex justify-center">
        <Image
          className="object-contain w-full h-auto max-w-[800px]"
          src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1718305976063-WhatsApp%20Image%202024-06-13%20at%2021.06.27.jpeg"
          alt=""
          sizes="100vw"
          width={1280}
          height={663}
        />
      </div>

      <div className="col-feature mb-8 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <Link href="/prijdtancit/deti"><h2>Děti</h2></Link>

          <p>Přípravka tanečního sportu pro děti do 15ti let</p>
          <p>Viděli jste náborovou akci ve škole, reklamu nebo chcete, aby Vaše dítě tancovalo? Pak jste tu správně!</p>

          <Link href="/prijdtancit/deti" className={buttonCls({ size: 'lg', className: 'no-underline' })}>Více <ChevronRight /></Link>
        </div>

        <div className="prose prose-accent">
          <Link href="/prijdtancit/mladez"><h2>Mládež</h2></Link>

          <p>Taneční kurzy pro mládež</p>
          <p>Máš za sebou taneční? Nebo taneční teprve začínáš a bojíš se, že to nezvládneš? Pak pokračuj do sekce mládež a zjisti víc!</p>

          <Link href="/prijdtancit/mladez" className={buttonCls({ size: 'lg', className: 'no-underline' })}>Více <ChevronRight /></Link>
        </div>
      </div>
    </Layout>
  );
};

export default Page;
