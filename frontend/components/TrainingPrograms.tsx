import { Route } from 'nextjs-routes';
import { ServiceCard } from './cards/ServiceCard';

export const TrainingPrograms = () => {
  return (
    <div className="col-feature my-4">
      {useServices().map((x, i) => (
        <ServiceCard key={i} href={x.href} image={x.image} header={x.header}>
          {x.text}
        </ServiceCard>
      ))}
    </div>
  );
};

const useServices = (): {
  href: Route | Exclude<Route, { query: any }>['pathname'];
  image: string;
  header: string;
  text: string;
}[] => [
  {
    href: '/skolni-krouzky',
    image: '/images/services-pripravka.png',
    header: 'Kroužky Olymp Dance',
    text: 'Tanči s námi na tvé škole: základní krůčky jednoduše a pohodlně ve školních kroužcích na 22 školách v Olomouci, Prostějově a okolí.',
  },
  {
    href: {
      pathname: '/programy/[id]/[...slug]',
      query: { id: '1', slug: ['treninkovy-program-basic'] },
    },
    image: '/images/services-pro-deti.png',
    header: 'Tréninkový program Basic',
    text: 'Už od začátku s trenéry národního týmu - základy tanců, správné držení těla, kondiční průprava hravou formou pod vedením špičkových trenérů.',
  },
  {
    href: {
      pathname: '/programy/[id]/[...slug]',
      query: { id: '2', slug: ['treninkovy-program-sport'] },
    },
    image: '/images/services-pro-zacatecniky.png',
    header: 'Tréninkový program Sport',
    text: 'Tréninkový program pro výkonnostní sportovce s individuální přístupem, pravidelná soutěžní činnost.',
  },
  {
    href: {
      pathname: '/programy/[id]/[...slug]',
      query: { id: '3', slug: ['treninkovy-program-top'] },
    },
    image: '/images/services-soutezni.png',
    header: 'Tréninkový program TOP',
    text: 'Tréninkový program na úrovni vrcholových sportovců včetně tréninků s pravidelně zvanými externisty. Taneční sportovci na mistrovské úrovni.',
  },
];
