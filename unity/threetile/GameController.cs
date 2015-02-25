using UnityEngine;
using System.Collections;

public class GameController : MonoBehaviour
{
	public GameObject tileQuad;

	// Use this for initialization
	void Start ()
	{

		GameObject topLeft = Instantiate (tileQuad, new Vector3 (-1.1f, 1.1f), Quaternion.identity) as GameObject;
		topLeft.renderer.material.mainTextureOffset = new Vector2 (0f, 0.66f);
		GameObject topMiddle = Instantiate (tileQuad, new Vector3 (0f, 1.1f), Quaternion.identity) as GameObject;
		topMiddle.renderer.material.mainTextureOffset = new Vector2 (0.33f, 0.66f);
		GameObject topRight = Instantiate (tileQuad, new Vector3 (1.1f, 1.1f), Quaternion.identity) as GameObject;
		topRight.renderer.material.mainTextureOffset = new Vector2 (0.66f, 0.66f);

		GameObject left = Instantiate (tileQuad, new Vector3 (-1.1f, 0f), Quaternion.identity) as GameObject;
		left.renderer.material.mainTextureOffset = new Vector2 (0f, 0.33f);


	}

	
	// Update is called once per frame
	void Update ()
	{
	
	}
}
