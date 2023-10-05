import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import { buttonCls } from '@/ui/style';
import { ChevronRight } from 'lucide-react';

const Page = () => {
  return (
    <Layout showTopMenu>
      <NextSeo title="Přijď tančit!" />

      <div className="col-feature mt-8 text-center">
        <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1696525490988-TKOLYMP-nabor-FB-uvod-820x462.png" />
      </div>

      <div className="col-feature mb-8 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <a href="/prijdtancit/mladez"><h2>Děti</h2></a>

          <p>Přípravka tanečního sportu pro děti do 15ti let</p>
          <p>Viděli jste náborovou akci ve škole, reklamu nebo chcete, aby Vaše dítě tancovalo? Pak jste tu správně!</p>

          <a href="/prijdtancit/deti" className={buttonCls({ size: 'lg', className: 'no-underline' })}>Více <ChevronRight /></a>
        </div>

        <div className="prose prose-accent">
          <a href="/prijdtancit/mladez"><h2>Mládež</h2></a>

          <p>Taneční kurzy pro mládež</p>
          <p>Máš za sebou taneční? Nebo taneční teprve začínáš a bojíš se, že to nezvládneš? Pak pokračuj do sekce mládež a zjisti víc!</p>

          <a href="/prijdtancit/mladez" className={buttonCls({ size: 'lg', className: 'no-underline' })}>Více <ChevronRight /></a>
        </div>
      </div>
    </Layout>
  );
};

export default Page;
