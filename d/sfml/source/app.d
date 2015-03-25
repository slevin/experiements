import std.stdio;
import derelict.sfml2.window;
import derelict.sfml2.graphics;
import derelict.sfml2.system;

void main()
{

    DerelictSFML2System.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-system.2.2.dylib");
    DerelictSFML2Window.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-window.2.2.dylib");
    DerelictSFML2Graphics.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-graphics.2.2.dylib");

    sfVideoMode mode = sfVideoMode(800, 600, 32);
    sfRenderWindow *window = sfRenderWindow_create(mode, "SFML Window, yo", sfResize | sfClose, null);
    if (!window) { return; }

    sfTexture *texture = sfTexture_createFromFile("plane.png", null);
    if (!texture) { return; }

    sfSprite *sprite = sfSprite_create();
    sfSprite_setTexture(sprite, texture, sfTrue);


    sfEvent event;
    while(sfRenderWindow_isOpen(window)) {
        while(sfRenderWindow_pollEvent(window, &event)) {
            if (event.type == sfEvtClosed) {
                sfRenderWindow_close(window);
            }
        }

        sfRenderWindow_clear(window, sfBlack);

        sfRenderWindow_drawSprite(window, sprite, null);

        sfRenderWindow_display(window);
    }

    sfSprite_destroy(sprite);
    sfTexture_destroy(texture);
    sfRenderWindow_destroy(window);

}
