import React from "react";
import Head from "next/head"
import { useRouter } from "next/router"
import useDeepCompareEffect from 'use-deep-compare-effect';

interface MetaProps {
  title: string;
  canonical?: string;
}

const MetaContext = React.createContext<readonly [
  MetaProps,
  React.Dispatch<React.SetStateAction<MetaProps>>,
]>(undefined as any);

export const useMetaReadonly = () => {
  const data = React.useContext(MetaContext);
  if (data === undefined) {
    throw new Error('You must use `useMeta` from inside a provider');
  }
  return data[0];
}

export const useMeta = (props: MetaProps) => {
  const data = React.useContext(MetaContext);
  const router = useRouter();
  if (data === undefined) {
    throw new Error('You must use `useMeta` from inside a provider');
  }
  const [meta, setMeta] = data;
  useDeepCompareEffect(() => {
    setMeta(props);
    let canonical = props.canonical;
    if (canonical && !canonical.startsWith('/')) {
      canonical = '/' + canonical;
    }

    if (canonical && router.asPath !== canonical) {
      router.replace({ pathname: canonical }, undefined);
    }
  }, [props]);
  return meta;
}

export const ProvideMeta: React.FC = React.memo(function ProvideMeta({ children }) {
  const router = useRouter();
  const [meta, setMeta] = React.useState<MetaProps>({
    title: '',
  });

  const base = process.env.NEXT_PUBLIC_BASE_URL;
  let canonical = meta.canonical || `${router.asPath !== "/" ? router.asPath : ""}`;
  if (!canonical.startsWith('/')) {
    canonical = '/' + canonical;
  }

  return (
    <MetaContext.Provider value={[meta, setMeta]}>
      <Head>
        <link rel="canonical" href={`${base}${canonical}`} />
        <title>{meta?.title ? `${meta?.title} • ` : ''}TK Olymp</title>
        <meta charSet="utf-8" />
        <meta name="viewport" content="initial-scale=1.0, width=device-width" />
        <meta name="keywords" content="taneční klub, tk, olymp, olomouc, tk olymp, sportovní tanec" />
        <meta name="ICBM" content="49.591700,17.285174" />
        <meta name="geo.placename" content="Olomouc, Česká Republika" />
        <meta name="geo.position" content="49.591700;17.285174" />
        <meta name="geo.region" content="cs" />
        <meta name="wot-verification" content="ec0cf41ab42dae52d3d4" />
        <meta name="msvalidate.01" content="7BD6C8B5748FC22EF06AB3AE89900885" />
        <meta name="facebook-domain-verification" content="k8tt64a93roxiymxo79clpvklan9j2" />
        <meta name="google-site-verification" content="Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w" />
        <meta name="norton-safeweb-site-verification" content="r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" />
        <meta property="fb:app_id" content="132983570203245" />
        <link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
      </Head>
      {children}
    </MetaContext.Provider >
  );
});
