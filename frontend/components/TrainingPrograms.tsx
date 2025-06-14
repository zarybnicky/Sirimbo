import { ServiceCard } from '@/ui/ServiceCard';
import type { LinkProps } from 'next/link'
type Route = LinkProps['href'];

export function TrainingPrograms() {
  return (
    <div className="col-feature my-4">
      {services.map((x, i) => (
        <ServiceCard key={i} href={x.href} image={x.image} header={x.header}>
          {x.text}
        </ServiceCard>
      ))}
    </div>
  );
}

const services: {
  href: Route;
  image: string;
  header: string;
  text: string;
}[] = [
  {
    href: {
      pathname: '/treninkove-programy/[id]/[...slug]',
      query: { id: '1', slug: ['treninkovy-program-basic'] },
    },
    image: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915638-Karty-Basic.jpg',
    header: 'Tréninkový program Basic',
    text: 'Už od začátku s trenéry národního týmu - základy tanců, správné držení těla, kondiční průprava hravou formou pod vedením špičkových trenérů.',
  },
  {
    href: {
      pathname: '/treninkove-programy/[id]/[...slug]',
      query: { id: '2', slug: ['treninkovy-program-sport'] },
    },
    image: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915638-Karty-Sport.jpg',
    header: 'Tréninkový program Sport',
    text: 'Tréninkový program pro výkonnostní sportovce s individuální přístupem, pravidelná soutěžní činnost.',
  },
  {
    href: {
      pathname: '/treninkove-programy/[id]/[...slug]',
      query: { id: '3', slug: ['treninkovy-program-top'] },
    },
    image: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915638-Karty-TOP.jpg',
    header: 'Tréninkový program TOP',
    text: 'Tréninkový program na úrovni vrcholových sportovců včetně tréninků s pravidelně zvanými externisty. Taneční sportovci na mistrovské úrovni.',
  },
  {
    href: '/skolni-krouzky',
    image: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915637-Karty-OlympDance.jpg',
    header: 'Kroužky Olymp Dance',
    text: 'Tanči s námi na tvé škole: základní krůčky jednoduše a pohodlně ve školních kroužcích na 22 školách v Olomouci, Prostějově a okolí.',
  },
];
