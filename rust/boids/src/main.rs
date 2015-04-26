extern crate sfml;
extern crate nalgebra as na;

use sfml::system::Vector2f;
use sfml::window::{ContextSettings, VideoMode, event, Close};
use sfml::graphics::{RenderWindow, Texture, Sprite, Color};

use na::{Vec2};


fn main() {
    let mut env = SFMLEnv::new(800, 600);

    while env.win.is_open() {
        for event in env.win.events() {
            match event {
                event::Closed => env.win.close(),
                _ => { }
            }
        }

        env.clear();

        env.win.display();
    }
}

struct SFMLEnv {
    win: RenderWindow,
    width: u32,
    height: u32,
    shipTexture: Texture
}

impl SFMLEnv {
    fn new(width: u32, height: u32) -> SFMLEnv {
        let mut window = RenderWindow::new(VideoMode::new_init(width, height, 32),
                                           "SFML Example",
                                           Close,
                                           &ContextSettings::default())
            .expect("Cannot create a new Render Window.");
        window.set_vertical_sync_enabled(true);
        let mut t = Texture::new_from_file("plane.png")
            .expect("Cannot create Texture");
        t.set_smooth(true);
        SFMLEnv {
            win: window,
            width: width,
            height: height,
            shipTexture: t
        }
    }

    fn clear(&mut self) {
        self.win.clear(&Color::black());
    }
}

struct Boid<'a> {
    pos: Vec2<f32>,
    vel: Vec2<f32>,
    acc: Vec2<f32>,
    sprite: Sprite<'a>,
    maxSpeed: f32,
    maxSteer: f32
}

impl<'a> Boid<'a> {

    fn new(env: SFMLEnv) -> Boid<'a> {
        let mut s = Sprite::new_with_texture(env.shipTexture).expect("Cannot create ship sprite.");
        let rect = s.get_local_bounds();
        s.set_origin2f(rect.width / 2.0,
                       rect.height / 2.0);
        Boid {
            pos: Vec2::new(0.0, 0.0),
            vel: Vec2::new(0.0, 0.0),
            acc: Vec2::new(0.0, 0.0),
            sprite: s,
            maxSpeed: 5.0,
            maxSteer: 0.2
        }
    }

}
