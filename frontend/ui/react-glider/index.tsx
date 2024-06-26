import * as React from 'react';
import Glider from 'glider-js';
import type { GliderProps, GliderMethods, MakeGliderProps } from '@/ui/react-glider/types';

const makeGliderOptions: (
  props: MakeGliderProps & {
    nextButtonEl: HTMLElement | null;
    prevButtonEl: HTMLElement | null;
    dotsEl: HTMLElement | null;
  },
) => Glider.Options = ({
  arrows,
  hasArrows,
  dots,
  hasDots,
  nextButtonEl,
  prevButtonEl,
  dotsEl,
  ...restProps
}) => ({
  ...restProps,
  skipTrack: true,
  arrows:
    (hasArrows && {
      next: arrows?.next || nextButtonEl,
      prev: arrows?.prev || prevButtonEl,
    }) ||
    undefined,
  dots: (hasDots && dots) || dotsEl || undefined,
});

const GliderComponent = React.forwardRef(function GliderReact(
  props: GliderProps,
  ref: React.Ref<GliderMethods>,
) {
  const {
    id,
    containerElement,
    className,
    hasArrows,
    arrows,
    hasDots,
    dots,
    scrollToSlide,
    scrollToPage,
    iconLeft,
    iconRight,
    skipTrack,
    children,
    onLoad,
    onSlideVisible,
    onAnimated,
    onRemove,
    onRefresh,
    onAdd,
    onDestroy,
    onSlideHidden,
    ...restProps
  } = props;

  const prevButtonRef = React.useRef<HTMLButtonElement>(null);
  const nextButtonRef = React.useRef<HTMLButtonElement>(null);
  const dotsRef = React.useRef<HTMLDivElement>(null);
  const elementRef = React.useRef<HTMLDivElement | null>(null);
  const gliderRef = React.useRef<GliderMethods | null>(null);

  // initialize the glider
  const callbackRef = React.useCallback(
    (element: HTMLDivElement) => {
      elementRef.current = element;
      if (element && !gliderRef.current) {
        const glider = new Glider(
          element,
          makeGliderOptions({
            ...restProps,
            arrows,
            hasArrows,
            dots,
            hasDots,
            nextButtonEl: nextButtonRef.current,
            prevButtonEl: prevButtonRef.current,
            dotsEl: dotsRef.current,
          }),
        ) as GliderMethods;

        gliderRef.current = glider;

        if (onLoad) {
          onLoad.call(
            glider,
            new CustomEvent('glider-loaded', {
              detail: { target: element },
            }),
          );
        }

        if (scrollToSlide) {
          glider.scrollItem(scrollToSlide - 1);
        } else if (scrollToPage) {
          glider.scrollItem(scrollToPage - 1, true);
        }

        // bind event listeners
        const addEventListener = (
          event: string,
          fn: ((e: CustomEvent) => void) | undefined,
        ): void => {
          if (typeof fn === 'function') {
            element.addEventListener(event, fn as (evt: Event) => void);
          }
        };

        addEventListener('glider-slide-visible', onSlideVisible);
        addEventListener('glider-animated', onAnimated);
        addEventListener('glider-remove', onRemove);
        addEventListener('glider-refresh', onRefresh);
        addEventListener('glider-add', onAdd);
        addEventListener('glider-destroy', onDestroy);
        addEventListener('glider-slide-hidden', onSlideHidden);
      }
    },
    [
      arrows,
      dots,
      hasArrows,
      hasDots,
      onLoad,
      restProps,
      scrollToPage,
      scrollToSlide,
      onAdd,
      onAnimated,
      onDestroy,
      onRefresh,
      onRemove,
      onSlideHidden,
      onSlideVisible,
    ],
  );

  // when the props update, sync the glider
  React.useEffect(() => {
    if (gliderRef.current) {
      gliderRef.current.setOption(
        makeGliderOptions({
          ...restProps,
          arrows,
          hasArrows,
          dots,
          hasDots,
          nextButtonEl: nextButtonRef.current,
          prevButtonEl: prevButtonRef.current,
          dotsEl: dotsRef.current,
        }),
        true,
      );
      gliderRef.current.refresh(true);
    }
  }, [arrows, dots, hasArrows, hasDots, restProps]);

  // when the event listeners change, sync the glider
  React.useEffect(() => {
    if (elementRef.current) {
      const addEventListener = (
        event: string,
        fn: ((e: CustomEvent) => void) | undefined,
      ): void => {
        if (typeof fn === 'function') {
          elementRef.current?.addEventListener(event, fn as (evt: Event) => void);
        }
      };

      addEventListener('glider-slide-visible', onSlideVisible);
      addEventListener('glider-animated', onAnimated);
      addEventListener('glider-remove', onRemove);
      addEventListener('glider-refresh', onRefresh);
      addEventListener('glider-add', onAdd);
      addEventListener('glider-destroy', onDestroy);
      addEventListener('glider-slide-hidden', onSlideHidden);
    }
    return () => {
      const removeEventListener = (
        event: string,
        fn: ((e: CustomEvent) => void) | undefined,
      ) => {
        if (typeof fn === 'function') {
          elementRef.current?.removeEventListener(event, fn as (evt: Event) => void);
        }
      };

      removeEventListener('glider-slide-visible', onSlideVisible);
      removeEventListener('glider-animated', onAnimated);
      removeEventListener('glider-remove', onRemove);
      removeEventListener('glider-refresh', onRefresh);
      removeEventListener('glider-add', onAdd);
      removeEventListener('glider-destroy', onDestroy);
      removeEventListener('glider-slide-hidden', onSlideHidden);
    };
  }, [onAdd, onAnimated, onDestroy, onRefresh, onRemove, onSlideHidden, onSlideVisible]);

  // expose the glider instance to the user so they can call the methods too
  React.useImperativeHandle(ref, () => gliderRef.current as GliderMethods);

  const Element = containerElement || 'div';

  return (
    <Element className="glider-contain">
      {props.hasArrows && !arrows && (
        <button
          type="button"
          className="glider-prev"
          aria-label="Previous"
          ref={prevButtonRef}
        >
          {iconLeft || '«'}
        </button>
      )}

      <div id={id} className={className} ref={callbackRef}>
        {skipTrack ? children : <div>{children}</div>}
      </div>

      {hasDots && !dots && <div ref={dotsRef} />}

      {props.hasArrows && !arrows && (
        <button
          type="button"
          className="glider-next"
          aria-label="Next"
          ref={nextButtonRef}
        >
          {iconRight || '»'}
        </button>
      )}
    </Element>
  );
});

export default GliderComponent;
