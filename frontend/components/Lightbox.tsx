import * as React from 'react';
import { useSwipeable } from 'react-swipeable';
import { Card } from 'components/Card';
import { ChevronLeft, ChevronRight } from 'react-feather';
import { GalleryItem } from 'lib/data/use-gallery';
import { useRouter } from 'next/router';
import classNames from 'classnames';
import { Transition } from '@headlessui/react';
import * as DialogPrimitive from "@radix-ui/react-dialog";

export interface LightboxProps {
  dirHref: string;
  images: GalleryItem[];
  initial: string;
}

export const Lightbox = ({ dirHref, images, initial }: LightboxProps) => {
  const didMountRef = React.useRef(false);
  const router = useRouter();
  const initialIx = images.findIndex(x => x.id == initial);
  const [selected, setSelected] = React.useState(Math.max(initialIx, 0));

  React.useEffect(() => {
    if (!didMountRef.current) {
      didMountRef.current = true;
      return;
    }
    router.replace(images[selected]!.href);
  }, [selected]);

  const prevImage = React.useCallback(() => (
    setSelected(sel => (sel + images.length - 1) % images.length)
  ), [images]);

  const nextImage = React.useCallback(() => (
    setSelected(sel => (sel + 1) % images.length)
  ), [images]);

  const handleKeyDown = React.useCallback<React.KeyboardEventHandler>(e => {
    if (e.key === "ArrowRight" || e.key === "Right" || e.key === " ") {
      nextImage();
    } else if (e.key === "ArrowLeft" || e.key === "Left") {
      prevImage();
    } else if (e.key === "Escape" || e.key === "Esc") {
      router.replace(dirHref);
    }
  }, []);

  const handlers = useSwipeable({
    onSwipedLeft: () => nextImage(),
    onSwipedRight: () => prevImage(),
  });

  if (!images.length) return null;

  return (
    <DialogPrimitive.Root
      open={true}
      onOpenChange={(open) => !open ? router.replace(dirHref) : null}
    >
      <DialogPrimitive.Overlay
        forceMount
        className="fixed inset-0 z-20 bg-black/50"
      />
      <DialogPrimitive.Content forceMount onKeyDown={handleKeyDown}>
        {images.map((image, ix) => (
          ix !== selected ? null : (
            <Transition
              key={image.img}
              as={React.Fragment}
              enter="ease-out duration-300"
              enterFrom="opacity-0"
              enterTo="opacity-100"
              leave="ease-in duration-200"
              leaveFrom="opacity-100"
              leaveTo="opacity-0"
            >
              <div {...handlers}>
                <img src={image.img} alt={image.name} style={{
                  display: 'block',
                  maxWidth: '100%',
                  maxHeight: `calc(100vh - 130px)`,
                  objectFit: 'contain',
                }} />
                <Card>
                  <div className="text-center tracking-wide uppercase text-stone-700 text-xs">{image.name}</div>
                </Card>
              </div>
            </Transition>
          )
        ))}
        <button onClick={nextImage} style={{ left: 0 }} className={classNames(
          "button button-icon",
          "absolute top-1/2 bg-white/50 shadow-lg hover:bg-white/80 -translate-y-1/2 transform"
        )}>
          <ChevronLeft />
        </button>
        <button onClick={prevImage} style={{ right: 0 }} className={classNames(
          "button button-icon",
          "absolute top-1/2 bg-white/50 shadow-lg hover:bg-white/80 -translate-y-1/2 transform"
        )}>
          <ChevronRight />
        </button>
      </DialogPrimitive.Content>
    </DialogPrimitive.Root>
  );
};
